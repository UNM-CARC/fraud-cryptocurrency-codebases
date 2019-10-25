use clang::*;

pub fn printelem(e: clang::Entity, indent: u32) {
    for i in 0..indent {
        print!(" ");
    }
    println!("{:?}", e);
    for j in e.get_children().into_iter() {
        if j.get_kind() == 
        printelem(j, indent + 2);
    }
}

pub fn parsecpp() {
    // Acquire an instance of `Clang`
    let clang = Clang::new().unwrap();

    // Create a new `Index`
    let index = Index::new(&clang, false, false);

    // Parse a source file into a translation unit
    //let tu = index.parser("misc/simple.cpp").parse().unwrap();
    let tu = index.parser("misc/prime-number.cpp").parse().unwrap();

    printelem(tu.get_entity(), 0);
    // Get the structs in this translation unit
    //let tree = tu.get_entity().get_children().into_iter().collect::<Vec<_>>(); //.filter(|e| {
    let tree = tu.get_entity().get_children().into_iter().collect::<Vec<_>>(); //.filter(|e| {
        //e.get_kind() == EntityKind::StructDecl
    //}).collect::<Vec<_>>();
    println!("{:?}", tree);

    //for e_ in tree {
    //    let type_ =  e_.get_type().unwrap();
    //    let name_ =  e_.get_name().unwrap();
    //    let size = type_.get_sizeof().unwrap();
    //    println!("{:?}", name_);

    //    for field in struct_.get_children() {
    //        let name = field.get_name().unwrap();
    //        let offset = type_.get_offsetof(&name).unwrap();
    //        println!("    field: {:?} (offset: {} bits)", name, offset);
    //    }
    //}
}
