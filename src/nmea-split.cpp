
#include <cpp11.hpp>
using namespace cpp11;

#include <vector>
#include <memory>

[[cpp11::register]]
list cpp_nmea_split(list nmea, std::string split_chars) {
    writable::list out;
    out.reserve(5);

    for (R_xlen_t i = 0; i < nmea.size(); i++) {
        if (nmea[i] == R_NilValue) {
            continue;
        }

        raws item = nmea[i];
        unsigned char* vals = RAW(item);
        std::vector<R_xlen_t> start;
        std::vector<R_xlen_t> end;

        start.push_back(0);
        for (R_xlen_t j = 0; j < item.size(); j++) {
            for (size_t k = 0; k < split_chars.size(); k++) {
                if (vals[j] == split_chars[k]) {
                    start.push_back(j + 1);
                    if (start.size() > 0) {
                        end.push_back(j);
                    }
                }
            }
        }
        end.push_back(item.size());

        for (size_t j = 0; j < start.size(); j++) {
            if (j >= ((size_t) out.size())) {
                out.push_back(writable::list(nmea.size()));
            }
            writable::raws split_raw(end[j] - start[j]);
            memcpy(RAW(split_raw), vals + start[j], split_raw.size());
            SET_VECTOR_ELT(out[j], i, split_raw);
        }
    }

    return out;
}
