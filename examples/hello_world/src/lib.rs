#[zeeland::zeeland]
pub trait Api {
    async fn post(n: String) -> String {
        Ok(format!("Hello, {n}!"))
    }
}

struct Api_client {
    client: reqwest::Client,
    root: String,
}

#[derive(Debug, thiserror::Error)]
enum ClientError {
    #[error("reqwest error: {0}")]
    Reqwest(#[from] reqwest::Error),
}

#[zeeland::async_trait]
impl Api for Api_client {
    type Error = ClientError;

    async fn post(&self, n: String) -> Result<String, ClientError> {
        #[derive(serde::Serialize)]
        struct Body {
            n: String,
        }

        Ok(self.client
            .post(&format!("{}/post", self.root))
            .json(&Body { n })
            .send()
            .await?
            .error_for_status()?
            .text()
            .await?)
    }
}
