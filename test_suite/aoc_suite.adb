with Day.Test;

package body AOC_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (new Day.Test.Test);
      return Ret;
   end Suite;

end AOC_Suite;
