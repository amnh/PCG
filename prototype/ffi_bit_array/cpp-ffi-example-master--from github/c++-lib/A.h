#ifndef _H_A_
#define _H_A_

#ifdef __cplusplus

class A {
public:
  A(int ain): a(ain) { }
  ~A() { }
  void run(int b);

private:
  int a;
};

#endif

#endif
