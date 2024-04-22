
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package StringUtils is

   function Copy (Item : Unbounded_String;
                  Start : Integer; Len : Integer) return Unbounded_String;

end StringUtils;
