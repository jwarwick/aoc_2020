-- AOC 2020, Day 23

package Day is

  type Cup_Number is range 1..9;
  type Cup_Number_Mod is mod Cup_Number'Last + 1;

  type Cup_Index is range 0..8;
  type Cup_Index_Mod is mod Cup_Index'Last + 1;

  type Cup_Array is array(Cup_Index) of Cup_Number;

  function play(c : in Cup_Array; steps : in Natural) return String;
end Day;
