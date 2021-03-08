#include <cpp11.hpp>
using namespace cpp11;
namespace writable = cpp11::writable;

#include "nmea/source.hpp"

std::unique_ptr<Source> nmea_get_source(list src) {
    sexp obj = src["obj"];

    if (Rf_inherits(src, "nmea_src_raw")) {
        raws raw = (raws) obj;
        return std::unique_ptr<Source>(new SourceBytes(RAW(raw), raw.size()));
    } else if (Rf_inherits(src, "nmea_src_connection")) {
        return std::unique_ptr<Source>(new SourceConnection(obj));
    } else {
        stop("Source must be a raw vector");
    }
}

[[cpp11::register]]
list cpp_read_nmea(list obj,
                   std::string sentence_start, std::string sentence_end,
                   int max_length) {
    std::unique_ptr<Source> src = nmea_get_source(obj);

    // 65536 is what readr:::read_connection() uses by default
    Scanner scanner(src.get(), 65536);

    writable::list sentences;
    writable::integers offsets;

    // without these sentences and offsets will be null
    // in the case of zero sentences found
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
