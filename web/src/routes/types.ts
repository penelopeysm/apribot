export type Post = {
    id: string,
    time: Date,
    title: string,
    body: string,
    flair: string | null,
    url: string,
    submitter: string,
}
