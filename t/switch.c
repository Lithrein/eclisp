switch (a)
  {
  case 1:
    return 1;
  case 2:
  case 3:
    return 5;
  case 4:
    return (4 + 3);
  case 5:
  default:
    return getchar ();
  default:
    return (getchar () + getchar ());
  }
