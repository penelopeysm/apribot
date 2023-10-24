function pad(n: number) {
    return n < 10 ? `0${n}` : n;
}

export function showDate(d: Date) {
    // getMonth returns 0 for January (?!)
    return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}:${pad(d.getSeconds())}`;
}
