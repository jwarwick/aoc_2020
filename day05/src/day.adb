-- AoC 2020, Day 5
with Ada.Text_IO;
-- with Ada.Strings.Fixed; use Ada.Strings.Fixed;
-- with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Day is
  package TIO renames Ada.Text_IO;

  function parse_line(line : in String) return Boarding_Pass is
    curr : Boarding_Pass;
  begin
    declare
      low : Row_Type := Row_Type'first;
      high : Row_Type := Row_Type'last;
      tmp : Float;
    begin
      for idx in 0..6 loop
        tmp := (Float(high)-Float(low))/2.0;
        if line(line'first + idx) = 'B' then
          low := low + Row_Type(Float'Floor(tmp)) + 1;
          curr.Row := low;
        elsif line(line'first + idx) = 'F' then
          high := high - Row_Type(Float'Floor(tmp)) - 1;
          curr.Row := high;
        else
          TIO.put_line("Don't know that value: " & line(line'first + idx));
          return curr;
        end if;
      end loop;
    end;

    declare
      low : Seat_Type := Seat_Type'first;
      high : Seat_Type := Seat_Type'last;
      tmp : Float;
    begin
      for idx in 7..9 loop
      tmp := (Float(high)-Float(low))/2.0;
      if line(line'first + idx) = 'R' then
        low := low + Seat_Type(Float'Floor(tmp)) + 1;
        curr.Seat := low;
      elsif line(line'first + idx) = 'L' then
        high := high - Seat_Type(Float'Floor(tmp)) - 1;
        curr.Seat := high;
      else
        TIO.put_line("Don't know that value: " & line(line'first + idx));
        return curr;
      end if;
      end loop;
    end;
    return curr;
  end parse_line;

  function load_batch(filename : in String) return Boarding_Pass_Vectors.Vector is
    file : TIO.File_Type;
    output : Boarding_Pass_Vectors.Vector;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      declare
        curr : constant Boarding_Pass := parse_line(TIO.get_line(file));
      begin
        output.append(curr);
      end;
    end loop;
    TIO.close(file);
    return output;
  end load_batch;

  function seat_id(pass : in Boarding_Pass) return Natural is
  begin
    return (Natural(pass.row) * 8) + Natural(pass.seat);
  end seat_id;

  function highest_id(passes : in Boarding_Pass_Vectors.Vector) return Natural is
    max : Natural := 0;
    curr : Natural := 0;
  begin
    for p of passes loop
      curr := seat_id(p);
      if curr > max then
        max := curr;
      end if;
    end loop;
    return max;
  end highest_id;
end Day;
