-- generic pointer to an array with its len
struct array<elem> {
  len: int,
  unwrap: &elem,
}

-- command line arguments, can be the argument of 'main' function
type args = array<cstr>;

-- c string
type cstr = &char;

-- string slice; wrapped for being able to have own impl
struct str {
  unwrap: array<char>  
}

-- print to stdout
fn print(s: str) {
  write(1, s)
}

-- print to stdout with newline
fn println(s: str) {
  print(s);
  print("\n")
}

-- print error to stdout and exit with code 1
fn die(err: str) {
  print("error: ");
  println(err);
  exit(1)
}

-- error for not implemented yet parts
fn todo(what: str) {
  print("todo: ");
  println(what);
  exit(1)
}

-- compile source from given file to native with nasm & ld
fn main(args: args) {
  if args.len == 1 {
    die("incorrect argument count; usage: bpl path/to/file.bpl");
  }
  -- test
  let s: int = 0;
  while args.unwrap[0][s] != 0 {
    s += 1;
  }
  write(1, str { unwrap: array<char> { len: s, unwrap: args.unwrap[0] } });
}

