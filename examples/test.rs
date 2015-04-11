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

    {
        let section = i.section("User");
        println!("name={}", section.get("name").unwrap());
    }

    println!("");

    println!("conf[{}][{}]={}", "User", "name", i["User"]["name"]);
}
