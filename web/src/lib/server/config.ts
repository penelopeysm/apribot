import { env } from '$env/dynamic/private';

const onFly = env["FLY_APP_NAME"] !== undefined;

const dbUrl = onFly ? env["DATABASE_URL"] : env["FLY_PG_PROXY_CONN_STRING"];

if (dbUrl === undefined) {
    if (onFly) {
        throw new Error("DATABASE_URL not found when running on Fly.io. This should not happen");
    } else {
        throw new Error("Running locally, but FLY_PG_PROXY_CONN_STRING not set. Use `fly proxy 5432 -a apripsql`")
    }
}

const config = {
    onFly,
    dbUrl,
}
export default config;
