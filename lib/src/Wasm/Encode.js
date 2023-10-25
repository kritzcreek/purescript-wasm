export function bytes_of_float32(float) {
    const bytes = new Uint8Array(4)
    new DataView(bytes.buffer).setFloat32(0, float)
    return [bytes[3], bytes[2], bytes[1], bytes[0]]
}
