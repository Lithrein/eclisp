struct tag0
{
  int a;
  long long  b;
} sa;
typedef struct tag1
{
  int a;
  struct {
    int bla;
    float blo;
  } b;
  struct tag0 c;
} sb;
typedef struct {
  int a;
  struct {
    int bla;
    float blo;
  } b;
  struct tag0 c;
} sc;
typedef struct {
  int a;
  struct {
    int bla;
    float blo;
  } b;
  struct tag0 c;
};
struct {
  int a;
  struct {
    int bla;
    float blo;
  } b;
  struct tag0 c;
};
