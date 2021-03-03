
#include <stdexcept>
#include <string>
#include <sstream>
#include <cstring>
#include <vector>

class Source {
public:
    virtual ~Source() {}
    virtual void open() {}
    virtual size_t read(unsigned char* dst, size_t n_bytes) {
        return 0;
    }
    virtual void seek(size_t n) {}
    virtual void close() {}
};

class ParseableStringException: public std::runtime_error {
public:
  ParseableStringException(std::string expected, std::string found, const char* src, size_t pos):
    std::runtime_error(makeError(expected, found, src, pos)),
  expected(expected), found(found), src(src), pos(pos) {}

  std::string expected;
  std::string found;
  std::string src;
  size_t pos;

  static std::string makeError(std::string expected, std::string found, const char* src, size_t pos) {
    std::stringstream stream;
    stream << "Expected " << expected << " but found " << found << " (:" << pos << ")";
    return stream.str().c_str();
  }
};

class ParseableString {
public:
  ParseableString(Source* src, size_t buffer_size, const char* whitespace, const char* sep):
    src(src), 
    chars_vector(buffer_size),
    current_buffer_size(0), 
    buffer_size(buffer_size), 
    offset(0), 
    whitespace(whitespace), 
    sep(sep) {
      this->str = this->chars_vector.data();
    }

  size_t getMoreFromSource(size_t keeping = 0) {
    if (keeping > 0) {
      memmove(this->str, this->str + offset, keeping);
    }

    this->current_buffer_size = src->read(
      (unsigned char*)this->str + keeping, 
      this->buffer_size - keeping
    );
    this->offset = 0;
    return this->current_buffer_size;
  }

  void advance() {
      if (!this->finished()) {
          this->offset++;
      }
  }

  void advance(int n, size_t keeping = 0) {
    if ((this->offset + n) <= this->current_buffer_size) {
      this->offset += n;
      return;
    } 
    
    if (this->getMoreFromSource(keeping) == 0) {
        return;
    }

    this->advance(n);
  }

  bool finished(size_t keeping = 0) {
      if (this->offset < this->current_buffer_size) {
          return false;
      } else {
          return this->getMoreFromSource(keeping) > 0;
      }
  }

  // Returns the character at the cursor and advances the cursor
  // by one
  char readChar() {
    char out = this->peekChar();
    this->advance();
    return out;
  }

  // Returns the character currently ahead of the cursor
  // without advancing the cursor (skips whitespace)
  char peekChar() {
    this->skipWhitespace();
    if (this->finished()) {
        this->error("At least one character", "end of input");
    } else {
        return this->str[this->offset];
    }
  }

  // Returns true if the next character is one of `chars`
  bool is(char c) {
    return c == this->peekChar();
  }

  // Returns true if the next character is one of `chars`
  bool isOneOf(const char* chars) {
    return strchr(chars, this->peekChar()) != nullptr;
  }

  // Returns true if the next character is most likely to be a number
  bool isNumber() {
    // complicated by nan and inf
    if (this->isOneOf("-nNiI.")) {
      std::string text = this->peekUntilSep();
      try {
        std::stod(text);
        return true;
      } catch(std::exception& e) {
        return false;
      }
    } else {
      return this->isOneOf("-0123456789");
    }
  }

  // Returns true if the next character is a letter
  bool isLetter() {
    char found = this->peekChar();
    return (found >= 'a' && found <= 'z') || (found >= 'A' && found <= 'Z');
  }

  std::string assertWord() {
    std::string text = this->peekUntilSep();
    if (!this->isLetter()) {
      this->error("a word", quote(text));
    }

    this->advance(text.size());
    return text;
  }

  // Returns the integer currently ahead of the cursor,
  // throwing an exception if whatever is ahead of the
  // cursor cannot be parsed into an integer
  long assertInteger() {
    std::string text = this->peekUntilSep();
    const char* textPtr = text.c_str();
    char* endPtr;
    long out = std::strtol(textPtr, &endPtr, 10);
    if (endPtr != (textPtr + text.size())) {
      this->error("an integer", quote(text));
    }

    this->advance(text.size());
    return out;
  }

  // Returns the double currently ahead of the cursor,
  // throwing an exception if whatever is ahead of the
  // cursor cannot be parsed into a double. This will
  // accept "inf", "-inf", and "nan".
  double assertNumber() {
    if (this->finished()) {
      this->error("a number", "end of input");
    }

    std::string text = this->peekUntilSep();
    const char* textPtr = text.c_str();
    char* endPtr;
    double out = std::strtod(textPtr, &endPtr);
    if (endPtr != (textPtr + text.size())) {
      this->error("a number", quote(text));
    }

    this->advance(text.size());
    return out;
  }

  // Asserts that the character at the cursor is whitespace, and
  // returns a std::string of whitespace characters, advancing the
  // cursor to the end of the whitespace.
  std::string assertWhitespace() {
    if (this->finished()) {
      this->error("whitespace", "end of input");
    }

    char found = this->str[this->offset];
    if (strchr(this->whitespace, found) == nullptr) {
      this->error("whitespace", quote(this->peekUntilSep()));
    }

    size_t offset0 = this->offset;
    size_t nWhitespaceChars = this->skipWhitespace();
    return std::string(&(this->str[offset0]), nWhitespaceChars);
  }

  void assert_(char c) {
    char found = this->peekChar();
    if (found != c) {
      this->error(quote(c), quote(found));
    }
    this->advance();
  }

  // Asserts the that the character at the cursor is one of `chars`
  // and advances the cursor by one (throwing an exception otherwise).
  char assertOneOf(const char* chars) {
    char found = this->peekChar();

    if ((strlen(chars) > 0) && this->finished()) {
      this->error(expectedFromChars(chars), "end of input");
    } else if (strchr(chars, found) == nullptr) {
      this->error(expectedFromChars(chars), quote(this->peekUntilSep()));
    }

    this->advance();
    return found;
  }

  // Asserts that the cursor is at the end of the input
  void assertFinished() {
    this->assert_('\0');
  }

  // Returns the text between the cursor and the next separator,
  // which is defined to be whitespace or the following characters: =;,()
  // advancing the cursor. If we are at the end of the string, this will
  // return std::string("")
  std::string readUntilSep() {
    this->skipWhitespace();
    if (this->finished()) {
      return std::string("");
    }

    size_t wordLen = peekUntil(this->sep);
    std::string out(this->str + this->offset, wordLen);
    this->advance(wordLen);
    return out;
  }

  // Returns the text between the cursor and the next separator
  // (" \r\n\t,();=") without advancing the cursor.
  std::string peekUntilSep() {
    this->skipWhitespace();
    size_t wordLen = peekUntil(this->sep);
    if (wordLen == 0 && !this->finished()) {
      wordLen = 1;
    }
    return std::string(this->str + this->offset, wordLen);
  }

  // Advances the cursor past any whitespace, returning the
  // number of characters skipped.
  size_t skipWhitespace() {
    return this->skipChars(this->whitespace);
  }

  // Skips all of the characters in `chars`, returning the number of
  // characters skipped.
  size_t skipChars(const char* chars) {
    size_t n_skipped = 0;
    char c = this->str[this->offset];
    while (!(this->finished()) && strchr(chars, c)) {
      this->advance();
      n_skipped++;
      if (this->finished()) {
        break;
      }

      c = this->str[this->offset];
    }

    return n_skipped;
  }

  // Returns the number of characters until one of `chars` is encountered,
  // which may be 0.
  size_t peekUntil(const char* chars) {
    size_t n_advance = 0;
    char c;
    do {
      this->advance(1, n_advance);
      n_advance++;
      c = this->str[this->offset];
    } while(!(this->finished(n_advance)) && !strchr(chars, c));

    return n_advance;
  }

  [[ noreturn ]] void errorBefore(std::string expected, std::string found) {
    throw ParseableStringException(expected, quote(found), this->str, this->offset - found.size());
  }

  [[noreturn]] void error(std::string expected, std::string found) {
    throw ParseableStringException(expected, found, this->str, this->offset);
  }

  [[noreturn]] void error(std::string expected) {
    throw ParseableStringException(expected, quote(this->peekUntilSep()), this->str, this->offset);
  }

private:
  // a Source from which to read more data
  Source* src;
  // protects the memory for the underlying str buffer
  std::vector<char> chars_vector;
  // the current buffer
  char* str;
  // total number of valid characters in the current buffer
  size_t current_buffer_size;
  // total writable size of the buffer
  size_t buffer_size;
  // position of the cursor within the buffer
  size_t offset;
  // charaters considered whitespace
  const char* whitespace;
  // characters that act as delimeters
  const char* sep;

  // error helpers
  static std::string expectedFromChars(const char* chars) {
    size_t nChars = strlen(chars);
    std::stringstream stream;
    for (size_t i = 0; i < nChars; i++) {
      if (i > 0) {
        stream << " or ";
      }
      stream << quote(chars[i]);
    }

    return stream.str();
  }

  static std::string quote(std::string input) {
    if (input.size() == 0) {
      return "end of input";
    } else {
      std::stringstream stream;
      stream << "'" << input << "'";
      return stream.str();
    }
  }

  static std::string quote(char input) {
    if (input == '\0') {
      return "end of input";
    } else {
      std::stringstream stream;
      stream << "'" << input << "'";
      return stream.str();
    }
  }
};
