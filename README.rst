INI in RUST
-----------

INI_ is an informal standard for configuration files for some platforms or software. INI files are simple text files with a basic structure composed of "sections" and "properties".

.. _INI: http://en.wikipedia.org/wiki/INI_file

This is an INI file parser in Rust_.

.. _Rust: http://www.rust-lang.org/

Usage
=====

* Create a Ini configuration file.

.. code:: rust

    extern mod ini;
    use ini::Ini;

    fn main() {
        let mut conf = Ini::new();
        conf.begin_section(~"User")
            .set(~"given_name", ~"Tommy")
            .set(~"family_name", ~"Green")
            .end_section();
        conf.begin_section(~"Book")
            .set(~"name", ~"Rust cool")
            .end_section();
        conf.write_file("conf.ini");
    }

Then you will get ``conf.ini`` 

.. code:: ini

    [User]
    given_name=Tommy
    family_name=Green

    [Book]
    name=Rust cool

* Read from file ``conf.ini``

.. code:: rust

    extern mod ini;
    use ini::Ini;

    fn main() {
        let conf = Ini::load_from_file("conf.ini");
        conf.begin_section(~"User");
        let tommy = conf.get(~"given_name");
        let green = conf.get(~"family_name");
        conf.end_section();

        println!("{} {}", tommy, green);

        // iterating
        for (sec, prop) in conf.iter() {
            println!("Section: {}", sec);
            for (key, value) in prop.iter() {
                println!("{}:{}", key, value);
            }
        }
    }
