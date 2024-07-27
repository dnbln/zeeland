
#[zeeland::zeeland]
trait Api {
    fn post(&self, n: String) -> String;
}

impl Api for Api_impl {
    fn post(&self, n: String) -> String {
        format!("Hello, {n}!")
    }
}

#[rocket::launch]
fn rocket() -> _ {
    create_rocket()
}