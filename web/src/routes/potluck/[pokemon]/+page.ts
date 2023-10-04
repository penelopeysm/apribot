import { error } from '@sveltejs/kit';
import { potluckData } from '../data';

export function load({ params }) {
    const data = potluckData.get(params.pokemon);
    if (data === undefined) {
        throw error(404, `No potluck data found for '${params.pokemon}'!`);
    }
    return data;
}
