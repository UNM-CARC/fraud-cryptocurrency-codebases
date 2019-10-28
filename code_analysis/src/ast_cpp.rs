use clang::*;

pub fn printelem(e: clang::Entity, indent: u32) {
    for i in 0..indent {
        print!(" ");
    }
    println!("{:?}", e); //, e.get_mangled_name());
    for j in e.get_children().into_iter() {
        let kind = j.get_kind();
        match kind {
            EntityKind::TypedefDecl  => print!(""),
            //EntityKind::FunctionDecl => printelem(j, indent + 2),
            //EntityKind::StructDecl   => printelem(j, indent + 2),
            //EntityKind::VarDecl      => printelem(j, indent + 2),
            //EntityKind::ParmDecl     => printelem(j, indent + 2),
            //EntityKind::ClassDecl    => printelem(j, indent + 2),
            _                        => printelem(j, indent + 2),
        }
        //if j.get_kind() == EntityKind::FunctionDecl {
        //    printelem(j, indent + 2);
        //}
    }
}

pub fn serialize_ast(e1: clang::Entity) -> Vec<(clang::EntityKind, u32)> {

    //for 
}

pub fn parsecpp() {
    // Acquire an instance of `Clang`
    let clang = Clang::new().unwrap();

    // Create a new `Index`
    let index1 = Index::new(&clang, false, false);
    let index2 = Index::new(&clang, false, false);

    // Parse a source file into a translation unit
    //println!("{:?}", get_version()); // 9.0.0
    let tu1 = index1.parser("misc/simple.c").parse().unwrap();
    let tu2 = index2.parser("misc/simple2.c").parse().unwrap();
    //let tu = index.parser("misc/prime-number.cpp").parse().unwrap();

    //printelem(tu.get_entity(), 0);

    let serial1 = serialize_ast(tu1.get_entity());

    // Get the structs in this translation unit
    //let tree = tu.get_entity().get_children().into_iter().collect::<Vec<_>>(); //.filter(|e| {
    let tree = tu1.get_entity().get_children().into_iter().collect::<Vec<_>>(); //.filter(|e| {
    let tree = tu2.get_entity().get_children().into_iter().collect::<Vec<_>>(); //.filter(|e| {
        //e.get_kind() == EntityKind::StructDecl
    //}).collect::<Vec<_>>();
    //println!("{:?}", tree);

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
