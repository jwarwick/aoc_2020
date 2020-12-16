-- AoC 2020, Day 16
with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Day is
  package TIO renames Ada.Text_IO;

  procedure parse_ticket(line : in String; rules : in out Value_Vectors.Vector) is
    rest : Unbounded_String := to_unbounded_string(line);
  begin
    loop
      declare
        idx : constant Natural := index(rest, ",");
      begin
        if idx = 0 then
          rules.append(Natural'Value(to_string(rest)));
          exit;
        end if;
        rules.append(Natural'Value(slice(rest, 1, idx-1)));
        rest := to_unbounded_string(slice(rest, idx+1, length(rest)));
      end;
    end loop;
  end parse_ticket;

  procedure parse_rule(line : in String; rules : in out Rule_Vectors.Vector) is
    fs : constant Natural := index(line, ":") + 1;
    ss : constant Natural := index(line(fs+1 .. line'last), " ");
    ts : constant Natural := index(line(ss+1 .. line'last), " ");
    rule1 : constant String := line(fs+1 .. ss-1);
    h1 : constant Natural := index(rule1, "-");
    min1 : constant Natural := Natural'Value(rule1(rule1'first .. h1-1));
    max1 : constant Natural := Natural'Value(rule1(h1+1 .. rule1'last));
    rule2 : constant String := line(ts+1 .. line'last);
    h2 : constant Natural := index(rule2, "-");
    min2 : constant Natural := Natural'Value(rule2(rule2'first .. h2-1));
    max2 : constant Natural := Natural'Value(rule2(h2+1 .. rule2'last));
  begin
    rules.append(Field_Rule'(Min => min1, Max => max1));
    rules.append(Field_Rule'(Min => min2, Max => max2));
  end parse_rule;

  function load_file(filename : in String) return Tickets is
    file : TIO.File_Type;
    rules : Rule_Vectors.Vector := Rule_Vectors.Empty_Vector;
    values : Value_Vectors.Vector := Value_Vectors.Empty_Vector;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    loop
      declare
        line : constant String := TIO.get_line(file);
      begin
        if line = "" then
          exit;
        end if;
        parse_rule(line, rules);
      end;
    end loop;

    -- My ticket
    pragma Warnings (Off, "constant ""line"" is not referenced");
    declare
      line : constant String := TIO.get_line(file);
    begin
      null;
    end;
    pragma Warnings (On, "constant ""line"" is not referenced");

    -- My ticket values
    declare
      line : constant String := TIO.get_line(file);
    begin
      TIO.put_line("Mine: " & line);
    end;
        
    -- My ticket
    pragma Warnings (Off, "constant ""line"" is not referenced");
    pragma Warnings (Off, "constant ""line2"" is not referenced");
    declare
      line : constant String := TIO.get_line(file);
      line2 : constant String := TIO.get_line(file);
    begin
      null;
    end;
    pragma Warnings (On, "constant ""line"" is not referenced");
    pragma Warnings (On, "constant ""line2"" is not referenced");

    while not TIO.end_of_file(file) loop
      declare
        line : constant String := TIO.get_line(file);
      begin
        parse_ticket(line, values);
      end;
    end loop;

    TIO.close(file);
    return Tickets'(Rules => rules, Values => values);
  end load_file;

  function sum_error_rate(t : in Tickets) return Natural is
    sum : Natural := 0;
    found : Boolean := false;
  begin
    for v of t.Values loop
      found := false;
      for r of t.Rules loop
        if v >= r.Min and v <= r.Max then
          found := true;
          exit;
        end if;
      end loop;
      if not found then
        sum := sum + v;
      end if;
    end loop;
    return sum;
  end sum_error_rate;
end Day;
