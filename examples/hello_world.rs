#[zeeland::zeeland]
trait Api {
    async fn post(&self, n: String) -> String;
}

#[zeeland::async_trait]
impl Api for Api_impl {
    async fn post(&self, n: String) -> String {
        format!("Hello, {n}!")
    }
}

#[rocket::launch]
fn rocket() -> _ {
    create_rocket()
}

pub struct Api_client {
    client: reqwest::Client,
    root: String,
}

#[zeeland::async_trait]
impl Api for Api_client {
    async fn post(&self, n: String) -> String {
        #[derive(serde::Serialize)]
        struct Body {
            n: String,
        }

        self.client
            .post(&format!("{}/post", self.root))
            .json(&Body { n })
            .send()
            .await
            .unwrap()
            .text()
            .await
            .unwrap()
    }
}
