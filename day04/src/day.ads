-- AOC 2020, Day 4
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Hash;

package Day is

  package Passport_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

  function "="(Left, Right:Passport_Maps.Map) return Boolean;

  package Passport_Vectors is new Ada.Containers.Vectors
    (Index_Type   => Natural,
    Element_Type => Passport_Maps.Map);

  -- type Passports is access Passport_Vectors.Vector;

  function load_batch(filename : in String) return Passport_Vectors.Vector;
  function valid_count(batch : in Passport_Vectors.Vector) return Count_Type;


end Day;
