#include <iostream>
#include <string>
#include <cstdlib>

void printInt(int x) {
  std::cout << x << "\n";
}

void printString(const char* s) {
  std::cout << std::string(s) << "\n";
}

void error() {
  std::cout << "runtime error";
  exit(1);
}

int readInt() {
  int x;
  std::cin >> x;
  return x;
}

char* copyString(std::string s) {
  char* sc = (char*) malloc(s.length()+1);
  for (int i = 0; i < s.length(); i++)
    sc[i] = s[i];
  sc[s.length()] = 0;
  return sc;
}

char* readString() {
  std::string s;
  char c;
  int i = 0;
  while ((c = std::cin.get()) != '\n' || i == 0) {
    if (c != '\n')
      s += c;
    i++;
  }
  return copyString(s);
}

void* alloc(int size) {
  return (void*)malloc(size);
}

char* concat(char* s1, char* s2) {
  return copyString(std::string(s1)+std::string(s2));
}
