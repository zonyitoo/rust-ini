extern mod ini;

use ini::Ini;

fn main() {
    /*
    let mut conf = Ini::new();
    conf.begin_section(~"User")
        .set(~"name", ~"Raspberry树莓")
        .set(~"value", ~"Pi")
        .end_section();
    conf.begin_section(~"Library")
        .set(~"name", ~"Sun Yat-sen U")
        .set(~"location", ~"Guangzhou=world")
        .end_section();
    conf.write_file("conf.ini");
    */

    let i = Ini::load_from_file("conf.ini");
    for (sec, prop) in i.iter() {
        println!("Section: {}", *sec);
        for (k, v) in prop.iter() {
            println!("{}:{}", *k, *v);
        }
    }
}
