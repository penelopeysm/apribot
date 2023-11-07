export type UnsafePost = {
    unsafeId: string,
    unsafeTime: Date,
    unsafeTitle: string,
    unsafeBody: string,
    unsafeFlair: string | null,
    unsafeUrl: string,
    unsafeSubmitter: string,
}

export type Post = {
    id: string,
    time: Date,
    title: string,
    body: string,
    flair: string | null,
    url: string,
    submitter: string,
}

export type Vote = {
    post: Post,
    vote: boolean,
}

export type Name = {
    id: number,
    name: string,
    form: string | null,
    unique_name: string,
}
