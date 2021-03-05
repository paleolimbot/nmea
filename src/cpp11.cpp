// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"

// format-nmea.cpp
strings cpp_nmea_as_character(list nmea, bool ascii);
extern "C" SEXP _nmea_cpp_nmea_as_character(SEXP nmea, SEXP ascii) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_nmea_as_character(cpp11::as_cpp<cpp11::decay_t<list>>(nmea), cpp11::as_cpp<cpp11::decay_t<bool>>(ascii)));
  END_CPP11
}
// read-nmea.cpp
list cpp_read_nmea(SEXP obj, std::string senetence_start, std::string sentence_end, int max_length);
extern "C" SEXP _nmea_cpp_read_nmea(SEXP obj, SEXP senetence_start, SEXP sentence_end, SEXP max_length) {
  BEGIN_CPP11
    return cpp11::as_sexp(cpp_read_nmea(cpp11::as_cpp<cpp11::decay_t<SEXP>>(obj), cpp11::as_cpp<cpp11::decay_t<std::string>>(senetence_start), cpp11::as_cpp<cpp11::decay_t<std::string>>(sentence_end), cpp11::as_cpp<cpp11::decay_t<int>>(max_length)));
  END_CPP11
}

extern "C" {
/* .Call calls */
extern SEXP _nmea_cpp_nmea_as_character(SEXP, SEXP);
extern SEXP _nmea_cpp_read_nmea(SEXP, SEXP, SEXP, SEXP);
extern SEXP nmea_c_character_as_nmea(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_nmea_cpp_nmea_as_character", (DL_FUNC) &_nmea_cpp_nmea_as_character, 2},
    {"_nmea_cpp_read_nmea",         (DL_FUNC) &_nmea_cpp_read_nmea,         4},
    {"nmea_c_character_as_nmea",    (DL_FUNC) &nmea_c_character_as_nmea,    1},
    {NULL, NULL, 0}
};
}

extern "C" void R_init_nmea(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}