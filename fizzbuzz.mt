let
    var count : Integer;
    var i : Integer := 1;
    fun mod(n : Integer, d : Integer) : Integer = n - ((n / d) * d)
in
begin
    getint(count);
    while i <= count do begin
        if mod(i, 15) == 0 then
            printint(71228022)
        else if mod(i, 3) == 0 then
            printint(7122)
        else if mod(i, 5) == 0 then
            printint(8022)
        else
            printint(i);
        i := i + 1
    end
end