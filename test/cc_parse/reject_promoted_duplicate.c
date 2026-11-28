struct Record {
    int duplicate;
    union { int other; struct { int duplicate; }; };
};
