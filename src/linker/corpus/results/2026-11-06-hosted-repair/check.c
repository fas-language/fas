__thread int tls=7; int value=35; __attribute__((constructor)) static void init(void){value=35;} int main(void){return tls+value==42 ? 0 : 1;}
