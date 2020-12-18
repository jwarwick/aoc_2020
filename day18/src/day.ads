-- AOC 2020, Day 18

package Day is

  function hw_sum(filename : in String) return Long_Integer;

  private
  function eval_string(expr : in String) return Long_Integer;

end Day;
