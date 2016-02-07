%% grid definitions

-type grid_type() :: array:array(array:array(term())).
-type grid_type(Element) :: array:array(array:array(Element)).
-type point_type() :: {Row :: pos_integer(), Column :: pos_integer()}.