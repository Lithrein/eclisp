__asm__ volatile (
  "1...\n\t"
  "2...\n\t"
  "3...\n\t"
  "\n\t\n\t"
  : "+r" (sym3)
  : "z" (sym0), "i" (sym1), "j" (sym2)
  : "%si", "%al", "%cl", "%rbx", "%eax"
);
__asm__ volatile (
  "1..."
  "2..."
  "3..."
  :
  :
  :
);
