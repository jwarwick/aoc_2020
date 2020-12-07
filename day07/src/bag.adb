with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Bag is
  package TIO renames Ada.Text_IO;

  function "<" (Left, Right : Bag_Combo) return Boolean is
  begin
    return Left.Color < Right.Color;
  end "<";

  function test_data return Bag_Maps.Map is
    d : constant Bag_Maps.Map := parse_rules("test1.txt");
  begin
    return d;
  end test_data;

  function input_data return Bag_Maps.Map is
    d : constant Bag_Maps.Map := parse_rules("input.txt");
  begin
    return d;
  end input_data;

  function extract_color(line : in String; rest : out Unbounded_String) return Bag_Color is
    first_space : constant Natural := index(line, " ");
    second_space : constant Natural := index(line(first_space+1 .. line'last), " ");
    first_word : constant String := line(line'first .. first_space-1);
    second_word : constant String := line(first_space+1 .. second_space-1);
    ident : constant String := first_word & "_" & second_word;
    c : constant Bag_Color := Bag_Color'Value(ident);
  begin
    rest := to_unbounded_string(line(second_space+1..line'last));
    return c;
  end extract_color;

  procedure drop_one(line : in String; rest : out Unbounded_String) is
    first_space : constant Natural := index(line, " ");
  begin
    rest := to_unbounded_string(line(first_space+1..line'last));
  end drop_one;

  function get_next(uline : in out Unbounded_String) return Bag_Combo is
    line : constant String := to_string(uline);
    first_space : constant Natural := index(line, " ");
    num : constant Natural := Natural'Value(line(line'first .. first_space-1));
    s : constant String := line(first_space+1 .. line'last);
    color : constant Bag_Color := extract_color(s, uline);
  begin
    return Bag_Combo'(Color => color, Count => num);
  end get_next;

  function build_set(line : in String) return Bag_Sets.Set is
    s : Bag_Sets.Set := Empty_Set;
    rest : Unbounded_String := to_unbounded_string(line);
  begin
    if 'n' = line(line'first) then
      return s;
    end if;

    loop
      declare
        combo : constant Bag_Combo := get_next(rest);
      begin
        s.insert(combo);
        if 0 = index(to_string(rest), " ") then
          exit;
        end if;
        drop_one(to_string(rest), rest);
      end;
    end loop;

    return s;
  end build_set;

  -- procedure print(m : in Bag_Maps.Map) is
  -- begin
  --   for c in m.Iterate loop
  --     TIO.put_line(Bag_Color'Image(key(c)));
  --     for s of element(c) loop
  --       TIO.put_line("    " & Bag_Color'Image(s.Color) & ":" & Natural'Image(s.Count));
  --     end loop;
  --   end loop;
  -- end print;

  procedure parse_line(line : in String; m : in out Bag_Maps.Map) is
    rest : Unbounded_String;
    c : constant Bag_Color := extract_color(line, rest);
    s : Bag_Sets.Set;
  begin
    drop_one(to_string(rest), rest);
    drop_one(to_string(rest), rest);
    s := build_set(to_string(rest));
    m.insert(c, s);
  end parse_line;
  --
  function parse_rules(f : in String) return Bag_Maps.Map is
    file : TIO.File_Type;
    d : Bag_Maps.Map := Empty_Map;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => f);
    while not TIO.end_of_file(file) loop
      parse_line(TIO.get_line(file), d);
    end loop;
    TIO.close(file);
    -- print(d);
    return d;
  end parse_rules;
end Bag;
