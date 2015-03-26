extern crate ini;

use ini::Ini;

fn main() {

    let mut conf = Ini::new();
    conf.begin_section("User")
        .set("name", "Raspberry树莓")
        .set("value", "Pi")
        .end_section();
    conf.begin_section("Library")
        .set("name", "Sun Yat-sen U")
        .set("location", "Guangzhou=world\x0ahahaha")
        .end_section();
    conf.write_to_file("conf.conf").unwrap();

    let mut i = Ini::load_from_file("conf.conf").unwrap();
    for (sec, prop) in i.iter() {
        println!("Section: {}", *sec);
        for (k, v) in prop.iter() {
            println!("{}:{}", *k, *v);
        }
    }
    println!("");

    i.begin_section("User");
    {
        let name = i.get("name").unwrap();
        println!("name={}", name);
    }
    i.end_section();

    println!("");

    println!("conf[{}][{}]={}", "User", "name", i["User".to_string()][&"name".to_string()]);
}
