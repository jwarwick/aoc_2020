-- AoC 2020, Day 15
-- with Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

package body Day is
  -- package TIO renames Ada.Text_IO;

  function natural_hash(n : in Natural) return Hash_Type is
  begin
    return Hash_Type(n);
  end natural_hash;

  package Age_Vectors is new Ada.Containers.Vectors
    (Index_Type   => Natural,
    Element_Type => Natural);
  use Age_Vectors;

  package Natural_Hashed_Maps is new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type => Natural,
    Element_Type => Age_Vectors.Vector,
    Hash => Natural_Hash,
    Equivalent_Keys => "=");

  use Natural_Hashed_Maps;
  
  ages : Natural_Hashed_Maps.Map := Empty_Map;

  function sequence(s : in Input_Array; step : in Natural) return Natural is
    index : Natural := 1;
    last : Natural;
    vec : Age_Vectors.Vector := Empty_Vector;
    prev : Natural;
  begin
    clear(ages);

    for e of s loop
      clear(vec);
      vec.append(index);
      ages.insert(e, vec);
      last := e;
      index := index + 1;
    end loop;

    loop
      vec := ages(last);
      last := vec.last_element;
      if vec.length = 1 then
        last := 0;
      else
        prev := vec(vec.last_index - 1);
        last := last - prev;
      end if;

      if ages.contains(last) then
        vec := ages(last);
        vec.append(index);
        ages(last) := vec;
      else
        clear(vec);
        vec.append(index);
        ages.insert(last, vec);
      end if;

      if index = step then
        return last;
      end if;

      index := index + 1;
    end loop;
  end sequence;
end Day;
