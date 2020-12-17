-- AoC 2020, Day 16
with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

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
    comb : constant Combined_Rules := (Field_Rule'(Min => min1, Max => max1), Field_Rule'(Min => min2, Max => max2));
  begin
    rules.append(comb);
  end parse_rule;

  function load_file(filename : in String) return Tickets is
    file : TIO.File_Type;
    rules : Rule_Vectors.Vector := Rule_Vectors.Empty_Vector;
    values : Value_Vectors.Vector := Value_Vectors.Empty_Vector;
    nested : Nested_Vectors.Vector := Nested_Vectors.Empty_Vector;
    my_ticket : Value_Vectors.Vector := Value_Vectors.Empty_Vector;
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
      parse_ticket(line, my_ticket);
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
        values.clear;
        parse_ticket(line, values);
        nested.append(values);
      end;
    end loop;

    TIO.close(file);
    return Tickets'(Rules => rules, Values => nested, Ticket => my_ticket);
  end load_file;

  function sum_error_rate(t : in Tickets) return Natural is
    sum : Natural := 0;
    found : Boolean := false;
    curr : Natural := 0;
  begin
    for v of t.Values loop
      sub_loop:
      for subv of v loop
        found := false;
        curr := subv;
        for r of t.Rules loop
          if curr >= r(1).Min and curr <= r(1).Max then
            found := true;
            exit;
          end if;
          if curr >= r(2).Min and curr <= r(2).Max then
            found := true;
            exit;
          end if;
        end loop;
        if not found then
          sum := sum + curr;
        end if;
      end loop sub_loop;
    end loop;
    return sum;
  end sum_error_rate;

  procedure filter_values(t : in Tickets; valid : in out Nested_Vectors.Vector) is
    found : Boolean := false;
    drop_count : Natural := 0;
  begin
    for v of t.Values loop
      sub_loop:
      for subv of v loop
        found := false;
        for r of t.Rules loop
          if subv >= r(1).Min and subv <= r(1).Max then
            found := true;
            exit;
          end if;
          if subv >= r(2).Min and subv <= r(2).Max then
            found := true;
            exit;
          end if;
        end loop;
        if not found then
          exit;
        end if;
      end loop sub_loop;
      if found then
        valid.append(v);
      else
        drop_count := drop_count + 1;
      end if;
    end loop;
  end filter_values;

  package Rule_Sets is new Ada.Containers.Ordered_Sets
    (Element_Type => Natural);
  use Rule_Sets;

  function departure_fields(t : in Tickets) return Long_Integer is
    type Rules_Array is array(0 .. t.Rules.length-1) of Rule_Sets.Set;

    function all_single(a : in Rules_Array) return Boolean is
    begin
      for r of a loop
        if r.length /= 1 then
          return false;
        end if;
      end loop;
      return true;
    end all_single;

    function get_singles(a : in Rules_Array) return Rule_Sets.Set is
      singles : Rule_Sets.Set := Empty_Set;
    begin
      for r of a loop
        if r.length = 1 then
          singles := singles or r;
        end if;
      end loop;
      return singles;
    end get_singles;

    procedure remove_singles(singles : in Rule_Sets.Set; possibles : in out Rules_Array) is
    begin
      for c in possibles'range loop
        if possibles(c).length /= 1 then
          possibles(c) := possibles(c) - singles;
        end if;
      end loop;
    end remove_singles;

    valid : Nested_Vectors.Vector := Nested_Vectors.Empty_Vector;
    possible_rules : Rules_Array;
    full_rules : Rule_Sets.Set := Empty_Set;
  begin
    for i in 0..t.Rules.length-1 loop
      full_rules.include(Natural(i));
    end loop;
    for i in 0..t.Rules.length-1 loop
      possible_rules(i) := full_rules;
    end loop;

    filter_values(t, valid);

    for v of valid loop
      for subv in 0..v.length-1 loop
        for r in 0..t.Rules.length-1 loop
          declare
            curr : constant Natural := v(Natural(subv));
            r_idx : constant Natural := Natural(r);
            curr_rule : constant Combined_Rules := t.Rules(r_idx);
            r1_min : constant Natural := curr_rule(1).Min;
            r1_max : constant Natural := curr_rule(1).Max;
            r2_min : constant Natural := curr_rule(2).Min;
            r2_max : constant Natural := curr_rule(2).Max;
          begin
            if not((curr >= r1_min and curr <= r1_max) or (curr >= r2_min and curr <= r2_max)) then
              possible_rules(Count_Type(r_idx)).exclude(Natural(subv));
            end if;
          end;
        end loop;
      end loop;
    end loop;

    loop
      if all_single(possible_rules) then
        exit;
      end if;

      declare
        singles : constant Rule_Sets.Set := get_singles(possible_rules);
      begin
        remove_singles(singles, possible_rules);
      end;
    end loop;

    declare
      product : Long_Integer := 1;
    begin
      for idx in 0..possible_rules'length-1 loop
        declare
          s : constant Rule_Sets.Set := possible_rules(Count_Type(idx));
          offset : constant Natural := s(s.first);
          my_value : constant Natural := t.Ticket(offset);
        begin
          if idx < 6 then
            product := product * Long_Integer(my_value);
          end if;
        end;
      end loop;
      return product;
    end;
  end departure_fields;
end Day;
