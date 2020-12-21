-- AoC 2020, Day 21 
with Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
use Ada.Containers;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Hash;

package body Day is
  package TIO renames Ada.Text_IO;

  package Ingredient_Maps is new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type       => String,
    Element_Type    => Natural,
    Hash            => Ada.Strings.Hash,
    Equivalent_Keys => "=");
  use Ingredient_Maps;
  
  package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Unbounded_String);
  use String_Vectors;


  package List_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => String_Vectors.Vector);
  use List_Vectors;

  package String_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Unbounded_String);
   use String_Sets;

  package Set_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => String_Sets.Set);
  use Set_Vectors;

  package Ingredient_To_Allergen_Maps is new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type       => String,
    Element_Type    => String_Sets.Set,
    Hash            => Ada.Strings.Hash,
    Equivalent_Keys => "=");
  use Ingredient_To_Allergen_Maps;

  package Allergen_To_Ingredient_Maps is new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type       => String,
    Element_Type    => Set_Vectors.Vector,
    Hash            => Ada.Strings.Hash,
    Equivalent_Keys => "=");
  use Allergen_To_Ingredient_Maps;

  counts : Ingredient_Maps.Map;
  foods : List_Vectors.Vector;
  allergen_keys : String_Sets.Set;
  ingredient_allergens : Ingredient_To_Allergen_Maps.Map;
  allergen_ingredients : Allergen_To_Ingredient_Maps.Map;
  allergens : Ingredient_To_Allergen_Maps.Map;

  -- function Allergen_Less(a1: in Unbounded_String; a2 : in Unbounded_String) return boolean is
  -- begin
  --   return allergens(to_string(a1)).first_element < allergens(to_string(a2)).first_element;
  -- end Allergen_Less;

  -- package Ingredient_Sort is new String_Vectors.Generic_Sorting(Allergen_Less);
  -- use Ingredient_Sort;

  procedure flatten_allergens is
    s : String_Sets.Set;
  begin
    for c in allergen_ingredients.Iterate loop
      s.clear;
      for v of allergen_ingredients(c) loop
        s := s or v;
      end loop;
      allergens.insert(key(c), s);
    end loop;
  end flatten_allergens;

  function all_length_one return Boolean is
  begin
    for c in allergens.iterate loop
      if allergens(c).length /= 1 then
        return false;
      end if;
    end loop;
    return true;
  end all_length_one;

  function all_singles return String_Sets.Set is
    s : String_Sets.Set := String_Sets.Empty_Set;
  begin
    for c in allergens.iterate loop
      if allergens(c).length = 1 then
        s.include(allergens(c).first_element);
      end if;
    end loop;
    return s;
  end all_singles;

  procedure reduce_allergens is
  begin
    loop
      if all_length_one then
        exit;
      end if;
      declare
        singles : constant String_Sets.Set := all_singles;
      begin
        if singles.is_empty then
          TIO.put_line("No single element sets!");
          exit;
        end if;
        for k of allergen_keys loop
          if allergens(to_string(k)).length > 1 then
            allergens(to_string(k)) := allergens(to_string(k)) - singles;
          end if;
        end loop;
      end;
    end loop;
  end reduce_allergens;

  procedure link_allergen(allergen : in String; ing : in String_Vectors.Vector) is
    set_vec : Set_Vectors.Vector := Set_Vectors.Empty_Vector;
    str_set : String_Sets.Set := String_Sets.Empty_Set;
  begin
    allergen_keys.include(to_unbounded_string(allergen));
    for i of ing loop
      str_set.include(i);
    end loop;
    set_vec.append(str_set);
    if allergen_ingredients.contains(allergen) then
      append(allergen_ingredients(allergen), set_vec);
    else
      allergen_ingredients.insert(allergen, set_vec);
    end if;

    for i of ing loop
      declare
        allergen_set : String_Sets.Set := String_Sets.Empty_Set;
        i_str : constant String := to_string(i);
      begin
        allergen_set.include(to_unbounded_string(allergen));
        if ingredient_allergens.contains(i_str) then
          ingredient_allergens(i_str).include(to_unbounded_string(allergen));
        else
          ingredient_allergens.insert(i_str, allergen_set);
        end if;
      end;
    end loop;
  end link_allergen;

  procedure read_ingredient(line : in String) is
    ing : String_Vectors.Vector := String_Vectors.Empty_Vector;
    idx : Natural := line'first;
  begin
    loop
      declare
        next_idx : constant Natural := index(line(idx..line'last), " ");
        s : constant String := line(idx..next_idx-1);
      begin
        idx := next_idx + 1;
        if s(s'first) = '(' then
          exit;
        end if;
        if counts.contains(s) then
          counts(s) := counts(s) + 1;
        else
          counts.insert(s, 1);
        end if;
        ing.append(to_unbounded_string(s));
      end;
    end loop;
    foods.append(ing);

    loop
      declare
        next_idx : constant Natural := index(line(idx..line'last), ",");
      begin
        if next_idx = 0 then
          link_allergen(line(idx..line'last-1), ing);
          exit;
        end if;
        link_allergen(line(idx..next_idx-1), ing);
        idx := next_idx + 2;
      end;
    end loop;
  end read_ingredient;

  procedure ingredient_count(filename : in String) is
    file : TIO.File_Type;
    sum : Natural := 0;
  begin
    foods.clear;
    counts.clear;
    ingredient_allergens.clear;
    allergen_ingredients.clear;
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      read_ingredient(TIO.get_line(file));
    end loop;
    TIO.close(file);

    -- TIO.put_line("Foods: ");
    -- for f of foods loop
    --   for i of f loop
    --     TIO.put(to_string(i) & ", ");
    --   end loop;
    --   TIO.new_line;
    -- end loop;

    -- for c in allergen_ingredients.Iterate loop
    --   TIO.put_line("Allergen: " & Key(c));
    --   for i of allergen_ingredients(c) loop
    --     for s of i loop
    --       TIO.put(to_string(s) & ", ");
    --     end loop;
    --     TIO.new_line;
    --   end loop;
    -- end loop;

    -- for c in ingredient_allergens.Iterate loop
    --   TIO.put_line("Ingredient: " & Key(c));
    --   for a of ingredient_allergens(c) loop
    --     TIO.put(to_string(a) & ", ");
    --   end loop;
    --   TIO.new_line;
    -- end loop;

    flatten_allergens;

    for c in ingredient_allergens.Iterate loop
      declare
        possible_allergens : constant String_Sets.Set := ingredient_allergens(c);
        ing : constant String := Key(c);
        cnt : Natural := Natural(possible_allergens.length);
      begin
        for a of possible_allergens loop
          for v of allergen_ingredients(to_string(a)) loop
            if not v.contains(to_unbounded_string(ing)) then
              cnt := cnt - 1;
              allergens(to_string(a)).exclude(to_unbounded_string(ing));
              exit;
            end if;
          end loop;
        end loop;
        if cnt = 0 then
          sum := sum + counts(ing);
        end if;
      end;
    end loop;
    TIO.put_line("Part 1: " & sum'IMAGE);

    reduce_allergens;

    for c in allergens.iterate loop
      TIO.put_line("Allergen: " & key(c));
      for elt of allergens(c) loop
        TIO.put(to_string(elt) & ", ");
      end loop;
      TIO.new_line;
    end loop;
    -- screw sorting in Ada.
    -- examine the above output and sort it yourself

  end ingredient_count;
end Day;
