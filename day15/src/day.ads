-- AOC 2020, Day 15

package Day is
  type Input_Array is array(Natural range <>) of Natural;

  function sequence(s : in Input_Array; step : in Natural) return Natural;

end Day;
