int main() {
  token input;
  token output;
  int i;
  int j;
  channel *s_in = create_buffer_nonblocking(4);
  channel *s_out = create_buffer_nonblocking(2);
  channel *s_1 = create_buffer_nonblocking(1);
  channel *s_1_delay = create_buffer_nonblocking(1);
  writeToken(s_1, 0);
  while (1) {
    actor11SDF(2, 1, s_in, s_1, f_1);
  }
  x[0] = 3;
  x[1].foo = 2;
  if (i < n) {
    return;
  }
  if (i > 0) {
    return 1;
  } else {
    return 0;
  }
  return 0;
  return;
  for (int i = 0; i < n; i++) {
    printf("%d\n", i);
  }
  fifo &fifo = f;
  printf("Hi");
  a->funca();
  a.getB().printB();
  p[i]->value;
  pthread_mutex_lock(&fifo->lock);
  int value = *p;
  *p = 10;
  int input[2];
}
static void actor11SDF(int consum, int prod, channel *ch_in, channel *ch_out,
                       void (*)(token *, token *) f) {}
