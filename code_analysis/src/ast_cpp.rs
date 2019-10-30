use clang::*;

fn printelem(e: clang::Entity) { //, indent: u32) {
    let mut stack: Vec<clang::Entity> = Vec::new();
    stack.push(e);

    //let mut sum: usize = e.get_children().len();
    while stack.is_empty() == false {
        let node: clang::Entity = *stack.last().unwrap();
        stack.pop();
        println!("{:?}", node.get_kind());
        for i in node.get_children().into_iter().rev() {
            stack.push(i);
        }
        //for i in 0..indent {
        //    print!(" ");
        //}
        //println!("{:?}", e.get_kind()); //, e.get_mangled_name());

    }
//    for j in e.get_children().into_iter() {
//        let kind = j.get_kind();
//        //let mut children: usize = 0;
//        match kind {
//            EntityKind::TypedefDecl  => print!(""),
//            //EntityKind::FunctionDecl => printelem(j, indent + 2),
//            //EntityKind::StructDecl   => printelem(j, indent + 2),
//            //EntityKind::VarDecl      => printelem(j, indent + 2),
//            //EntityKind::ParmDecl     => printelem(j, indent + 2),
//            //EntityKind::ClassDecl    => printelem(j, indent + 2),
//            _                        => printelem(j, indent + 2),
//        }
//        //sum += children;
//        //if j.get_kind() == EntityKind::FunctionDecl {
//        //    printelem(j, indent + 2);
//        //}
//    }

//    for i in 0..indent {
//        print!(" ");
//    }
//    println!("{:?}, {:?}", e.get_kind(), sum);
    //return sum;
}

//fn serialize_tail(e: clang::Entity, x: Vec<(clang::EntityKind, u32)) 
//                                    -> Vec<(clang::EntityKind, u32)> {
//    for i in e.get_children().into_iter() {
//        let kind = i.get_kind();
//        match kind {
//            EntityKind::TypedefDecl => return serialize_tail(i, x),
//            _                       => return serialize_tail(i, x.push((kind, )))
//        }
//    }
//    return 
//}

//fn serialize_ast<'a>(e: clang::Entity<'a>, xs: &'a mut Vec<(clang::Entity<'a>, usize)>) 
//                                            -> &'a mut Vec<(clang::Entity<'a>, usize)> {
fn serialize_ast(e: clang::Entity) -> Vec<(clang::Entity, usize)> {

    let mut stack: Vec<clang::Entity> = Vec::new();
    let mut out: Vec<(clang::Entity, usize)> = Vec::new();
    stack.push(e);
    let mut sum: usize = e.get_children().len();

    while stack.is_empty() == false {
        let node: clang::Entity = *stack.last().unwrap();
        stack.pop();
        //println!("{:?}", node.get_kind());

        for i in node.get_children().into_iter().rev() {
            stack.push(i);
        }
    }
    return out

    //for i in e.get_children().into_iter().rev() {
    //    let kind = i.get_kind();
    //    let mut children: usize = 0;
    //    match kind {
    //        //EntityKind::TypedefDecl  => children = 0,
    //        //EntityKind::ObjCProtocolExpr => children = 0,
    //        //EntityKind::FunctionDecl => children = serialize_ast(i, xs).last().unwrap().1,
    //        //EntityKind::StructDecl   => children = serialize_ast(i, xs).last().unwrap().1,
    //        //EntityKind::VarDecl      => children = serialize_ast(i, xs).last().unwrap().1,
    //        //EntityKind::ParmDecl     => children = serialize_ast(i, xs).last().unwrap().1,
    //        //EntityKind::ClassDecl    => children = serialize_ast(i, xs).last().unwrap().1,
    //        _                        => children = serialize_ast(i, &mut *xs).last().unwrap().1,
    //    }
    //    sum += children;
    //}
    //xs.push((e, sum));
    //return xs
}

pub fn parsecpp() {
    // Acquire an instance of `Clang`
    let clang = Clang::new().unwrap();

    // Create a new `Index`
    let index1 = Index::new(&clang, false, false);
    //let index2 = Index::new(&clang, false, false);

    // Parse a source file into a translation unit
    //println!("{:?}", get_version()); // 9.0.0
    //let tu1 = index1.parser("misc/simple.cpp").parse().unwrap();
    //let tu1 = index1.parser("/tmp/bitcoin/src/chain.cpp").parse().unwrap();
    let tu1 = index1.parser("misc/prime-number.cpp").parse().unwrap();
    //let tu1 = index1.parser("/tmp/bitcoin/src/hash.cpp").parse().unwrap();
    //let tu2 = index2.parser("misc/simple2.c").parse().unwrap();

    //printelem(tu1.get_entity(), 0);
    //printelem(tu1.get_entity());

    //let mut tmp: Vec<(clang::Entity, usize)> = Vec::new();

    let serial1 = serialize_ast(tu1.get_entity());// , &mut tmp);
    //println!("{:?}", serial1);
    for i in serial1.into_iter().rev() {
        if i.0.evaluate() != Some(clang::EvaluationResult::Unexposed) {
            println!("{:?}", i.1);
        }
    }

    // Get the structs in this translation unit
    //let tree = tu.get_entity().get_children().into_iter().collect::<Vec<_>>(); //.filter(|e| {
//    let tree = tu1.get_entity().get_children().into_iter().collect::<Vec<_>>(); //.filter(|e| {
//    let tree = tu1.get_entity().get_children().into_iter().filter(|e| { // .collect::<Vec<_>>()
//        e.get_kind() != EntityKind::TypedefDecl
//    }).collect::<Vec<_>>();
//    println!("{:?}", tree);

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
