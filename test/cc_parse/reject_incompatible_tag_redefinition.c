struct record { int count; char bytes[]; };
struct record { int count; char bytes[0]; };
struct record { int count; char bytes[1]; };
