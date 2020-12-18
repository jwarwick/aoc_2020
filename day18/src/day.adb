-- AoC 2020, Day 18
with Ada.Text_IO;

package body Day is
  package TIO renames Ada.Text_IO;

  type Operation is (none, add, mult);

  function eval_op(op : in Operation; left, right : in Long_Integer) return Long_Integer is
  begin
    case op is
      when none => return 0;
      when add => return left + right;
      when mult => return left * right;
    end case;
  end eval_op;

  function eval_partial_string(expr : in String; idx : in out Natural) return Long_Integer is
    left : Long_Integer := 0;
    tmp : Long_Integer;
    op : Operation := none;
  begin
    -- all numbers are a single digit
    while idx <= expr'last loop
      declare
        c : constant Character := expr(idx);
      begin
        idx := idx + 1;
        case c is
          when ' ' => null;
          when '+' => op := add;
          when '*' => op := mult;
          when '(' =>
            tmp := eval_partial_string(expr, idx);
            if op = none then
              left := tmp;
            else
              left := eval_op(op, left, tmp);
            end if;
          when ')' =>
            return left;
          when others =>
            if op = none then
              left := Long_Integer'Value((1 => c));
            else
              left := eval_op(op, left, Long_Integer'Value((1 => c)));
            end if;
        end case;
      end;
    end loop;
    return left;
  end eval_partial_string;

  function eval_string(expr : in String) return Long_Integer is
    start : Natural := expr'first;
  begin
    return eval_partial_string(expr, start);
  end eval_string;

  function hw_sum(filename : in String) return Long_Integer is
    file : TIO.File_Type;
    sum : Long_Integer := 0;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      sum := sum + eval_string(TIO.get_line(file));
    end loop;
    TIO.close(file);
    return sum;
  end hw_sum;

end Day;
