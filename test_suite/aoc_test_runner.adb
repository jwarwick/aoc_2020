with AUnit.Reporter.Text;
with AUnit.Run;
with AOC_Suite; use AOC_Suite;

procedure AOC_Test_Runner is
   procedure Runner is new AUnit.Run.Test_Runner (Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Runner (Reporter);
end AOC_Test_Runner;
