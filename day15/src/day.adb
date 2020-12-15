-- AoC 2020, Day 15
-- with Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers; use Ada.Containers;

package body Day is
  -- package TIO renames Ada.Text_IO;

  function natural_hash(n : in Natural) return Hash_Type is
  begin
    return Hash_Type(n);
  end natural_hash;

  package Natural_Hashed_Maps is new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type => Natural,
    Element_Type => Natural,
    Hash => Natural_Hash,
    Equivalent_Keys => "=");

  use Natural_Hashed_Maps;
  
  last_age : Natural_Hashed_Maps.Map := Empty_Map;
  previous_age : Natural_Hashed_Maps.Map := Empty_Map;

  function sequence(s : in Input_Array; step : in Natural) return Natural is
    index : Natural := 1;
    last : Natural;
    prev : Natural;
    diff : Natural;
  begin
    clear(last_age);
    clear(previous_age);

    for e of s loop
      last_age.insert(e, index);
      last := e;
      index := index + 1;
    end loop;

    loop
      if not previous_age.contains(last) then
        diff := 0;
      else
        prev := previous_age(last);
        last := last_age(last);
        diff := last - prev;
      end if;

      if last_age.contains(diff) then
        previous_age.include(diff, last_age(diff));
      end if;
      last_age.include(diff, index);
      last := diff;

      if index = step then
        return last;
      end if;

      index := index + 1;
    end loop;
  end sequence;
end Day;
