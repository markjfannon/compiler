let
    fun fib(n : Integer) : Integer = n < 0
        ? 1 / 0
        : n == 0
            ? 0
            : n == 1 || n == 2
                ? 1
                : fib(n - 1) + fib(n - 2);
    var input : Integer;
    var i : Integer := 1
in
begin
    getint(input);
    while i <= input do begin
        printint(fib(i));
        i := i + 1
    end
end
