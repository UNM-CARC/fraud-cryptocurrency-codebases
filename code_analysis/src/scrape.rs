use select::document::Document;
use select::predicate::Name;
use std::iter::FilterMap;

pub fn scraper(url: &str) -> Vec<String> {
    let resp = reqwest::get(url).unwrap();
    let mut html: Vec<String> = Vec::new();
    //let html = reqwest::get(url)?.text()?;
    assert!(resp.status().is_success());

    let out = Document::from_read(resp)
        .unwrap()
        .find(Name("a"))
        .filter_map(|n| n.attr("href"))
        .filter(|x| !x.contains("markets"))
        .filter(|x| !x.contains("24-hour"))
        .filter(|x| x.contains("/currencies/"))
        .for_each(|x| html.push(String::from(x)));
    return html;
}

pub fn scraper_coins(url: &str) -> Vec<String> {
    let resp = reqwest::get(url).unwrap();
    let mut html: Vec<String> = Vec::new();
    //let html = reqwest::get(url)?.text()?;
    assert!(resp.status().is_success());

    let out = Document::from_read(resp)
        .unwrap()
        .find(Name("a"))
        .filter_map(|n| n.attr("href"))
        //.filter(|x| !x.contains("markets"))
        //.filter(|x| !x.contains("24-hour"))
        //.filter(|x| x.contains("/currencies/"))
        .for_each(|x| html.push(String::from(x)));
    return html;
}
