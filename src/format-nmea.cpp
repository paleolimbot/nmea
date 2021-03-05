
#include <cpp11.hpp>
using namespace cpp11;
namespace writable = cpp11::writable;

#include <sstream>
#include <iomanip>

[[cpp11::register]]
strings cpp_nmea_as_character(list nmea, bool ascii) {
    R_xlen_t size = nmea.size();
    writable::strings result(size);

    // these are the printable values
    unsigned char min_char = 32;
    unsigned char max_char = 126;

    // for export to true character vector, use the whole range 
    // except the null character
    if (ascii) {
        min_char = 0x01;
        max_char = 0xff;
    }

    std::stringstream stream;
    unsigned char c;

    for (R_xlen_t i = 0; i < size; i++) {
        if (nmea[i] == R_NilValue) {
            result[i] = NA_STRING;
        } else {
            stream.str("");
            raws item = nmea[i];
            for (R_xlen_t j = 0; j < item.size(); j++) {
                c = item[j];
                if (c >= min_char && c <= max_char) {
                    stream << c;
                } else if (c == '\n') {
                    stream << "\\n";
                } else if(c == '\r') {
                    stream << "\\r";
                } else {
                    stream << "\\" << std::setfill('0') << std::setw(3) << ((int) c);
                }
            }
            result[i] = stream.str();
        }
    }

    return result;
}
