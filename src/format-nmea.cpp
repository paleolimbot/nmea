
#include <cpp11.hpp>
using namespace cpp11;
namespace writable = cpp11::writable;

#include <sstream>

[[cpp11::register]]
strings cpp_nmea_as_character(list nmea) {
    R_xlen_t size = nmea.size();
    writable::strings result(size);

    std::stringstream stream;

    for (R_xlen_t i = 0; i < size; i++) {
        if (nmea[i] == R_NilValue) {
            result[i] = NA_STRING;
        } else {
            stream.str("");
            raws item = nmea[i];
            for (R_xlen_t j = 0; j < item.size(); j++) {
                stream << (unsigned char) item[j];
            }
            result[i] = stream.str();
        }
    }

    return result;
}
