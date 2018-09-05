-module(reward).
-export([get/1]).
get(1)->{reward,1,30};
get(2)->{reward,2,30};
get(3)->{reward,3,30};
get(4)->{reward,4,40};
get(5)->{reward,5,50};
get(6)->{reward,6,60};
get(7)->{reward,7,70};
get(_)->{}.