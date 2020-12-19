-- AoC 2020, Day 19
with Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
use Ada.Containers;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Day is
  package TIO renames Ada.Text_IO;

  type Rule_Type is (term, nonterm);

  package Rule_Vectors is new Ada.Containers.Vectors
    (Index_Type => Natural,
    Element_Type => Natural);
  use Rule_Vectors;

  package Nested_Rule_Vectors is new Ada.Containers.Vectors
    (Index_Type => Natural,
    Element_Type => Rule_Vectors.Vector);
  use Nested_Rule_Vectors;

  type Rule(Kind : Rule_Type := nonterm) is record
    case Kind is
      when term =>
        Value : Character;
      when nonterm =>
        Rules : Nested_Rule_Vectors.Vector;
    end case;
  end record;

  procedure put(r : in Rule) is
  begin
    case r.Kind is
      when term =>
        TIO.put("Terminal: " & r.Value);
      when nonterm =>
        for v in r.Rules.first_index .. r.Rules.last_index loop
          for s of r.Rules(v) loop
            TIO.put(s'IMAGE & " ");
          end loop;
          if v /= r.Rules.last_index then
            TIO.put(" | ");
          end if;
        end loop;
    end case;
  end put;

  function rule_hash(id: Natural) return Hash_Type is
  begin
    return Hash_Type(id);
  end rule_hash;

  package Rule_Maps is new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type       => Natural,
    Element_Type    => Rule,
    Hash            => rule_hash,
    Equivalent_Keys => "=");

  use Rule_Maps;
  rules : Rule_Maps.Map := Empty_Map;

  pragma Warnings (Off, "procedure ""put_line"" is not referenced");
  procedure put_line(r : in Rule_Maps.Map) is
  pragma Warnings (On, "procedure ""put_line"" is not referenced");
  begin
    for c in r.Iterate loop
      TIO.put(key(c)'IMAGE & " => ");
      put(element(c));
      TIO.new_line;
    end loop;
  end put_line;

  function parse_transitions(line : in String) return Nested_Rule_Vectors.Vector is
    n : Nested_Rule_Vectors.Vector := Nested_Rule_Vectors.Empty_Vector;
    elt : Rule_Vectors.Vector := Rule_Vectors.Empty_Vector;
    idx : Natural := line'first;
  begin
    loop
      if idx > line'last then
        n.append(elt);
        exit;
      end if;
      if line(idx) = ' ' then
        idx := idx + 1;
      elsif line(idx) = '|' then
        n.append(elt);
        elt.clear;
        idx := idx + 1;
      else
        declare
          space_idx : constant Natural := index(line(idx .. line'last), " ");
        begin
          if space_idx = 0 then
            elt.append(Natural'Value(line(idx..line'last)));
            n.append(elt);
            exit;
          end if;
          elt.append(Natural'Value(line(idx..space_idx)));
          idx := space_idx + 1;
        end;
      end if;
    end loop;
    return n;
  end parse_transitions;

  procedure parse_rule(line : in String) is
    colon : constant Natural := index(line, ":");
    key : constant Natural := Natural'Value(line(line'first .. colon-1));
    start : constant Natural := colon + 2;
  begin
    if line(start) = '"' then
      rules.insert(key, Rule'(Kind => term, Value => line(start+1)));
    else
      declare
        n : constant Nested_Rule_Vectors.Vector := parse_transitions(line(start .. line'last));
      begin
        rules.insert(key, Rule'(Kind => nonterm, Rules => n));
      end;
    end if;
  end parse_rule;

  function match(line : in String; r : Rule_Vectors.Vector) return Boolean is
  begin
    if line'length /= r.length then
      return false;
    end if;
    if line'length /= 1 then
      return false;
    end if;
    declare
      f : constant Rule := rules(r.first_element);
      c : constant Character := line(line'first);
    begin
      if f.Kind /= term then
        return false;
      end if;
      if f.Value /= c then
        return false;
      end if;
    end;
    return true;
  end match;

  function test_input(line : in String; r : Rule_Vectors.Vector) return Boolean is
  begin
    if r.is_empty then
      return false;
    end if;
    if line'length = 0 then
      return false;
    end if;
    if match(line, r) then
      return true;
    end if;
    declare
      curr : constant Rule := rules(r.first_element);
    begin
      case curr.Kind is
        when term =>
          if curr.Value = line(line'first) then
            declare
              rest : Rule_Vectors.Vector := r;
            begin
              rest.delete_first;
              return test_input(line(line'first+1..line'last), rest);
            end;
          else
            return false;
          end if;
        when nonterm =>
          for v of curr.Rules loop
            declare
              rest : Rule_Vectors.Vector := r;
            begin
              rest.delete_first;
              if test_input(line, v & rest) then
                return true;
              end if;
          end;
          end loop;
          return false;
      end case;
    end;
  end test_input;

  function test_input(line : in String) return Boolean is
    r : Rule_Vectors.Vector := Rule_Vectors.Empty_Vector;
  begin
    r.append(0);
    return test_input(line, r);
  end test_input;

  function count_valid(filename : in String) return Natural is
    file : TIO.File_Type;
    sum : Natural := 0;
  begin
    rules.clear;
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    -- parse rules
    loop
      declare
        line : constant String := TIO.get_line(file);
      begin
        if line'length = 0 then
          exit;
        end if;
        parse_rule(line);
      end;
    end loop;

    -- TIO.put_line("Rules:");
    -- put_line(rules);

    -- inputs
    while not TIO.end_of_file(file) loop
      if test_input(TIO.get_line(file)) then
        sum := sum + 1;
      end if;
    end loop;
    TIO.close(file);
    return sum;
  end count_valid;
end Day;
