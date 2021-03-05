#include <cpp11.hpp>
using namespace cpp11;
namespace writable = cpp11::writable;

#include <memory>
#include <vector>
#include <sstream>

class Source {
public:
    virtual ~Source() {}
    virtual size_t read(unsigned char* dst, size_t n_bytes) {
        return 0;
    }
};

class SourceRaw: public Source {
public:
    SourceRaw(raws src): src(src), data(RAW(src.data())), offset(0), size(src.size()) {}

    size_t read(unsigned char* dst, size_t n_bytes) {
        check_user_interrupt();
        R_xlen_t new_offset = offset + n_bytes;
        if (new_offset >= size) {
            new_offset = size;
        }

        size_t n_cpy = new_offset - offset;
        if (n_cpy > 0) {
            memcpy(dst, data + offset, n_cpy);
        }
        this->offset = new_offset;

        return n_cpy;
    }

private:
    raws src;
    unsigned char* data;
    R_xlen_t offset;
    R_xlen_t size;
};

std::unique_ptr<Source> nmea_get_source(SEXP obj) {
    if (TYPEOF(obj) == RAWSXP) {
        return std::unique_ptr<Source>(new SourceRaw(obj));
    } else {
        stop("Source must be a raw vector");
    }
}

class NMEAScanner {
public:
  NMEAScanner(Source* src, size_t buffer_size):
    src(src), 
    current_buffer_size(0), 
    buffer_size(buffer_size), 
    offset(0) {
      this->str = (char*) malloc(buffer_size);
    }

    ~NMEAScanner() {
        free(this->str);
    }

    std::string read_until(const std::string& needle, size_t max_size) {
        size_t needle_size = needle.size();
        if ((needle_size == 0) || (needle_size > 255)) {
            stop("needle size must be between 1 and 255");
        }

        size_t max_read = max_size - needle_size;
        char buffer[256];
        memset(buffer, 0, 256);
        std::stringstream out;

        for (size_t n_read = 0; n_read <= max_read; n_read++) {
            size_t n_read_buf = peek_n(buffer, needle_size);

            if ((needle == buffer) || (n_read_buf != needle_size) || (n_read == max_read)) {
                out << std::string(buffer, n_read_buf);
                this->offset += n_read_buf;
                break;
            } else {
                out << buffer[0];
                this->offset++;
            }
        }

        return out.str();
    }

    size_t skip_until(std::string& needle) {
        if (this->finished()) {
            return 0;
        }

        char buffer = '\0';
        size_t n_skip = 0;
        do {
            peek_n(&buffer, 1);
            for (size_t i = 0; i < needle.size(); i++) {
                if (buffer == needle[i]) {
                    return n_skip;
                }
            }
            n_skip++;
        } while(this->advance());
       
        return n_skip;
    }

    size_t peek_n(char* dest, size_t n) {
        if ((this->offset + n) > this->current_buffer_size) {
            size_t n_read = this->get_more_from_source(this->current_buffer_size - this->offset);
            if (n_read < n) {
                n = n_read;
            }
        }

        if (n > 0) {
            memcpy(dest, this->str + this->offset, n);
        }

        return n;
    }

    size_t get_more_from_source(size_t keeping = 0) {
        if (keeping > 0) {
            memmove(this->str, this->str + offset, keeping);
        }

        this->current_buffer_size = src->read(
            (unsigned char*) this->str + keeping, 
            this->buffer_size - keeping
        );
        this->offset = 0;
        return this->current_buffer_size;
    }

    bool advance() {
        if (!this->finished()) {
            this->offset++;
            return true;
        } else {
            return false;
        }
    }

    bool finished() {
        if (this->offset < this->current_buffer_size) {
            return false;
        } else {
            return this->get_more_from_source() == 0;
        }
    }

private:
    // a Source from which to read more data
    Source* src;
    // the current buffer
    char* str;
    // total number of valid characters in the current buffer
    size_t current_buffer_size;
    // total writable size of the buffer
    size_t buffer_size;
    // position of the cursor within the buffer
    size_t offset;
};

[[cpp11::register]]
list cpp_read_nmea(SEXP obj, 
                   std::string sentence_start, std::string sentence_end,
                   int max_length) {
    std::unique_ptr<Source> src = nmea_get_source(obj);
    NMEAScanner scanner(src.get(), 8096);

    writable::list sentences;
    writable::integers offsets;
    sentences.reserve(10);
    offsets.reserve(10);

    size_t offset = scanner.skip_until(sentence_start);

    while (!scanner.finished()) {
        std::string item = scanner.read_until(sentence_end, max_length);

        writable::raws item_raw(item);
        sentences.push_back(item_raw);
        offsets.push_back(offset);

        size_t n_skipped = scanner.skip_until(sentence_start);
        offset += (item.size() + n_skipped);
    }
    
    writable::list out = {offsets, sentences};
    out.names() = {"offset", "sentence"};
    
    return out;
}
