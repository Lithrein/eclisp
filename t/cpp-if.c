#if (((defined (A) == defined (C)) && (defined (C) == K)) || defined (B))
  1 + 2;

#elif (defined (A) && defined (B) && ((A == 2)))
  3 + 4;

#else
  (2 + 4 + (1 & 2) + 2 + ((2 - 7 - 3) * 9 * 10)) / 5;

#endif
#if (((defined (A) == defined (C)) && (defined (C) == K)) & defined (B))
  1 + 2;

#endif
