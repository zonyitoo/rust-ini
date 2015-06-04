extern crate ini;

use ini::Ini;

fn main() {

    let mut conf = Ini::new();
    conf.begin_section(None)
        .set("general", "value");
    conf.begin_section(Some("User"))
        .set("name", "Raspberry树莓")
        .set("value", "Pi");
    conf.begin_section(Some("Library"))
        .set("name", "Sun Yat-sen U")
        .set("location", "Guangzhou=world\x0ahahaha");
    conf.write_to_file("conf.conf").unwrap();

    let i = Ini::load_from_file("conf.conf").unwrap();
    for (sec, prop) in i.iter() {
        println!("Section: {:?}", sec);
        for (k, v) in prop.iter() {
            println!("{}:{}", *k, *v);
        }
    }
    println!("");

    {
        let section = i.section(Some("User")).unwrap();
        println!("name={}", section.get("name").unwrap());
    }

    println!("");

    println!("conf[{}][{}]={}", "User", "name", i["User"]["name"]);
}
