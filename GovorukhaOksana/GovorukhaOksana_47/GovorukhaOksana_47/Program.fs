module TTT
open Trees


type TwoTypedThree =
| Dnode of Tree * TwoTypedThree * TwoTypedThree
| Tnode of Tree * TwoTypedThree * TwoTypedThree * TwoTypedThree
| Nleaf of Tree


 


