#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <memory.h>

SEXP nmea_c_character_as_nmea(SEXP lst) {
    R_xlen_t size = Rf_xlength(lst);
    SEXP result = PROTECT(Rf_allocVector(VECSXP, size));

    SEXP item, new_item;
    const char* item_ptr;
    size_t item_len;
    for (R_xlen_t i = 0; i < size; i++) {
        item = STRING_ELT(lst, i);
        if (item == NA_STRING) {
            SET_VECTOR_ELT(result, i, R_NilValue);
        } else {
            item_ptr = CHAR(item);
            item_len = strlen(item_ptr);
            new_item = PROTECT(Rf_allocVector(RAWSXP, item_len));
            memcpy(RAW(new_item), (const unsigned char*) item_ptr, item_len);
            SET_VECTOR_ELT(result, i, new_item);
            UNPROTECT(1);
        }
    }

    UNPROTECT(1);
    return result;
}
