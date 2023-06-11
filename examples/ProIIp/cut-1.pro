"Excerpt from the Prolog II+ Reference Manual, Section 2.1, page R2-2"
"Available at: https://www.prolog-heritage.org/en/m2.html"

color(red) ->;
color(blue) ->;

size(big) ->;
size(small) ->;

choice1(x.y) -> color(x) size(y);
choice1("that's all") ->;

choice2(x.y) -> ! color(x) size(y);
choice2("that's all") ->;

choice3(x.y) -> color(x) ! size(y);
choice3("that's all") ->;

choice4(x.y) -> color(x) size(y) !;
choice4("that's all") ->;

-> choice1(u);
-> choice2(u);
-> choice3(u);
-> choice4(u);
-> choice1(u) !;
