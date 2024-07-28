#[zeeland::zeeland]
pub trait Api {
    async fn post(n: String) -> String {
        format!("Hello, {n}!")
    }
}

struct Api_client {
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
