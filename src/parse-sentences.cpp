#include <cpp11.hpp>
using namespace cpp11;
namespace writable = cpp11::writable;

#include "parseable-string.h"

class SourceRaw: public Source {
public:
    SourceRaw(raws src): src(src), data(RAW(src.data())), offset(0), size(src.size()) {}

    size_t read(unsigned char* dst, size_t n_bytes) {
        R_xlen_t new_offset = offset + n_bytes;
        if (new_offset >= size) {
            new_offset = size;
        }

        size_t n_cpy = new_offset - offset;
        if (n_cpy > 0) {
            memcpy(dst, data + offset, n_cpy);
        }

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

[[cpp11::register]]
void cpp_nmea_parse_sentences(SEXP obj, int skip, 
                              double n_max, double sentence_max_length) {
    if (n_max < 0) {
        n_max = INT_MAX;
    }

    // 82 is technically the max length of a sentence but in practice
    // extensions can make sentences much longer than this
    if (sentence_max_length < 82) {
        stop("Minimum sentence length must be >=82 bytes");
    }

    std::unique_ptr<Source> src = nmea_get_source(obj);
}
