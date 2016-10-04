proc count (): proc =
  var cnt: int
  return proc (): int =
    cnt = cnt + 1
    return cnt


var c = count()
echo c()
echo c()
