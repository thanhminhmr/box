#ifndef _BOX_
#define _BOX_

// ----------------------------------------------------------------------------
// Type define

typedef struct _BOX_CALLBACK_STRUCT {
	void(__cdecl *callback)(struct _BOX_CALLBACK_STRUCT*);
	void *handle;
	unsigned char *buffer;
	long size;
	long count;
} BOX_CALLBACK_STRUCT;

typedef void(__cdecl *BOX_CALLBACK)(struct _BOX_CALLBACK_STRUCT*);

// 
// Proto define

unsigned long __cdecl box_workmem_size();
void __cdecl box_compress(
	BOX_CALLBACK_STRUCT *_input, // input
	BOX_CALLBACK_STRUCT *_output, // output
	void *_mem // // workmem
);
void __cdecl box_decompress(
	BOX_CALLBACK_STRUCT *_input, // input
	BOX_CALLBACK_STRUCT *_output, // output
	void *_mem // // workmem
);

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// Internal things below, modify at your own risks

#ifdef NDEBUG

#if defined(_MSC_VER)
#define __inline__   __inline //__forceinline
#define __noinline__ __declspec(noinline)
#elif defined(__GNUC__)
#define __inline__   __attribute__((always_inline))
#define __noinline__ __attribute__((noinline))
#endif

#else // NDEBUG undefined

#if defined(_MSC_VER)
#define __inline__   __declspec(noinline)
#define __noinline__ __declspec(noinline)
#elif defined(__GNUC__)
#define __inline__   __attribute__((noinline))
#define __noinline__ __attribute__((noinline))
#endif

#endif // NDEBUG

// ----------------------------------------------------------------------------
// Constant & configure

#define MATCH_CNT 16
#define MATCH_MAX ((MATCH_CNT - 1) << 24)
#define MATCH_INC (1 << 24)

#define HASH_SHIFT 6
#define HASH_BITS (4 * HASH_SHIFT)
#define HASH_SIZE (1 << HASH_BITS)
#define HASH_MASK (HASH_SIZE - 1)

#define SM_PART 258
#define SM_SIZE (SM_PART * 256 * MATCH_CNT)

// ----------------------------------------------------------------------------
// Internal typedef

// 8, 16, 32 bit unsigned types (adjust as appropriate)
typedef unsigned char  U8;
typedef unsigned short U16;
typedef unsigned long  U32;

typedef struct _BOX_WORKMEM {
	U32 htbl[HASH_SIZE];
	U32 ptbl[SM_SIZE];
} BOX_WORKMEM;

// ----------------------------------------------------------------------------
// MISC

/* fill memory with pattern. */
template <class type>
static __inline__ void fillmem(type *pointer, int size, type fill) {
	for (int i = 0; i < size; i++) {
		pointer[i] = fill;
	}
}

// ----------------------------------------------------------------------------
// IO Base class

class box_io {
protected:
	/*************************************************************
	* Input & Output.
	*/
	BOX_CALLBACK_STRUCT *input, *output;

	/* fill input buffer */
	__inline__ void _fillbuf() {
		(*input->callback)(input);
	}

	/* flush output buffer */
	__inline__ void _flushbuf() {
		(*output->callback)(output);
	}

	/* get chr from input */
	__inline__ U32 _getchr() {
		if (input->count < input->size) { // is buffer not empty?
			return (U32) input->buffer[input->count++]; // get chr from buffer
		} else {
			_fillbuf(); // fill input buffer
			if (input->size > 0) {
				input->count = 0; // reset count
				return (U32) input->buffer[input->count++]; // get chr from buffer
			} else {
				return (U32) 0xFFFFFFFF; // end of input
			}
		}
	}

	/* put chr to output */
	__inline__ void _putchr(U32 chr) {
		if (output->count < output->size) { // is buffer not empty?
			output->buffer[output->count++] = (U8) chr; // put chr to buffer
		} else {
			_flushbuf(); // flush output buffer
			if (output->size > 0) {
				output->count = 0; // reset count
				output->buffer[output->count++] = (U8) chr; // put chr to buffer
			} else {
				// well, dead end here, we lost a chr
			}
		}
	}

	/* io initialize */
	__inline__ void io_init() {
		input->count = 0; // reset count
		output->count = 0; // reset count
		_fillbuf();
	}

	/* io finalize */
	__inline__ void io_final() {
		if (output->count != 0) {
			_flushbuf();
		}
	}

	__inline__ box_io(
		BOX_CALLBACK_STRUCT *_input, // input
		BOX_CALLBACK_STRUCT *_output // output
	) : input(_input), output(_output) {}
};

// ----------------------------------------------------------------------------
// MKBOX: Compress input to output.
class box_mkbox : public box_io {
protected:
	/* match table & prediction table */
	BOX_WORKMEM *mem;
	U32 *htbl;
	U32 *ptbl;

	/*************************************************************
	* Encoder: Arithmetic Encoding using StateMap prediction
	*/

	/* Range, initially [0, 1), scaled by 2^32 */
	U32 x1, x2;

	/* shift out identical leading bytes */
	__inline__ void _shift_out() {
		if ((x1 ^ x2) > 0x00FFFFFF) {
		} else {
			do {  // pass equal leading bytes of range
				_putchr(x1 >> 24);
				x1 <<= 8;
				x2 = (x2 << 8) | 0xFF;
			} while ((x1 ^ x2) <= 0x00FFFFFF);
		}
	}

	/* Flush first unequal byte of range */
	__inline__ void flush() {
		_putchr(x1 >> 24);
	}

	/* Compress bit (0..1) in context cxt (0..n-1) */
	__inline__ void code(U32 cxt, U32 bit) {
		U32 p = ptbl[cxt]; // prediction
		ptbl[cxt] += (bit << 25) - (p >> 7); // update prediction
		U32 xmid = x1 + ((x2 - x1) >> 12) * (p >> 20);
		*(bit ? &x2 : &x1) = xmid + (bit ? 0 : 1);
		//bit ? x2 = xmid : x1 = xmid + 1;
		_shift_out();
	}
	/* Compress bit 0 in context cxt (0..n-1) */
	__inline__ void code_zero(U32 cxt) {
		U32 p = ptbl[cxt]; // prediction
		ptbl[cxt] += (0 << 25) - (p >> 7); // update prediction
		x1 += ((x2 - x1) >> 12) * (p >> 20) + 1;
		_shift_out();
	}
	/* Compress bit 1 in context cxt (0..n-1) */
	__inline__ void code_one(U32 cxt) {
		U32 p = ptbl[cxt]; // prediction
		ptbl[cxt] += (1 << 25) - (p >> 7); // update prediction
		x2 = x1 + ((x2 - x1) >> 12) * (p >> 20);
		_shift_out();
	}

	/*************************************************************
	* Main compress routine
	*/

	/* code a literal */
	__inline__ void code_lit(U32 smc, U32 chr) {
		// code high 4 bits in contexts cxt+1..15
		U32 block = ((chr & 0xFF) >> 4) | 16;
		code(smc + 1, (block >> 3) & 1);
		code(smc + (block >> 3), (block >> 2) & 1);
		code(smc + (block >> 2), (block >> 1) & 1);
		code(smc + (block >> 1), block & 1);
		// code low 4 bits in one of 16 blocks of 15 cxts (to reduce cache misses)
		smc += 15 * (block - 15);
		block = (chr & 15) | 16;
		code(smc + 1, (block >> 3) & 1);
		code(smc + (block >> 3), (block >> 2) & 1);
		code(smc + (block >> 2), (block >> 1) & 1);
		code(smc + (block >> 1), block & 1);
	}

	/* do compress */
	__inline__ void compress() {
		x1 = 0; x2 = 0xFFFFFFFF;
		htbl = mem->htbl;
		ptbl = mem->ptbl;

		fillmem(htbl, HASH_SIZE, (U32) 0);
		fillmem(ptbl, SM_SIZE, (U32) 1 << 31);
		io_init();

		U32 hash = 0, smc = 0, cxt = 0, chr;
		while ((chr = _getchr()) <= 0xFF) {
			if (chr == (cxt & 0x0000FF)) {  // match first?
				cxt += (cxt < MATCH_MAX) ? MATCH_INC : 0;  // increment count
				code_zero(smc);
			} else if ((chr << 8) == (cxt & 0x00FF00)) {  // match second?
				cxt = (cxt & 0xFF0000) | ((cxt << 8) & 0x00FF00) | chr | MATCH_INC;
				code_one(smc);
				code_one(smc + 1);
				code_zero(smc + 2);
			} else if ((chr << 16) == (cxt & 0xFF0000)) {  // match third?
				cxt = ((cxt << 8) & 0xFFFF00) | chr | MATCH_INC;
				code_one(smc);
				code_one(smc + 1);
				code_one(smc + 2);
			} else {  // literal?
				cxt = ((cxt << 8) & 0xFFFF00) | chr;
				code_one(smc);
				code_zero(smc + 1);
				code_lit(smc + 2, chr);
			}
			htbl[hash] = cxt;
			hash = (((hash * 5) << HASH_SHIFT) + chr) & HASH_MASK;
			cxt = htbl[hash];
			smc = ((chr << 4) | (cxt >> 24)) * SM_PART;
		}
		// mark EOF by code first match as literal
		code_one(smc);
		code_zero(smc + 1);
		code_lit(smc + 2, cxt);
		flush();
		io_final();
	}

public:
	// Compress from in to out. out should be positioned past the header.
	__inline__ box_mkbox(
		BOX_CALLBACK_STRUCT *_input, // input
		BOX_CALLBACK_STRUCT *_output, // output
		BOX_WORKMEM *_mem // workmem
	) : box_io(_input, _output), mem(_mem) {
		compress();
	}
};

// ----------------------------------------------------------------------------
// UNBOX: Decompress input to output.
class box_unbox : public box_io {
protected:
	/* match table & prediction table */
	BOX_WORKMEM *mem;
	U32 *htbl;
	U32 *ptbl;

	/*************************************************************
	* Encoder: Arithmetic Encoding using StateMap prediction
	*/

	/* Range, initially [0, 1), scaled by 2^32 */
	U32 x1, x2;
	/* decode point */
	U32 x;

	/* shift in new trailing bytes */
	__inline__ void _shift_in() {
		while ((x1 ^ x2) <= 0x00FFFFFF) {  // pass equal leading bytes of range
			x1 <<= 8;
			x2 = (x2 << 8) | 0xFF;
			x = (x << 8) | (_getchr() & 0xFF);
		}
	}

	/* init x for decoding */
	__inline__ void fill_x() {
		for (int i = 0; i < 4; ++i) {
			x = (x << 8) | (_getchr() & 0xFF);
		}
	}

	/* Compress bit (0..1) in context cxt (0..n-1) */
	__inline__ U32 decode(U32 cxt) {
		U32 p = ptbl[cxt]; // prediction in high 25 bits
		U32 xmid = x1 + ((x2 - x1) >> 12) * (p >> 20);
		U32 bit = (x <= xmid) ? 1 : 0;
		ptbl[cxt] += (bit << 25) - (p >> 7); // update prediction
		*(bit ? &x2 : &x1) = xmid + (bit ? 0 : 1);
		//bit ? x2 = xmid : x1 = xmid + 1;
		_shift_in();
		return bit;
	}

	/*************************************************************
	* Main decompress routine
	*/

	/* decode a literal */
	__inline__ U32 decode_lit(U32 smc) {
		// decode high 4 bits in contexts cxt+1..15
		U32 hi = 2 + decode(smc + 1);
		hi += hi + decode(smc + hi);
		hi += hi + decode(smc + hi);
		hi += hi + decode(smc + hi);
		// decode low 4 bits in one of 16 blocks of 15 cxts
		smc += 15 * (hi - 15);
		U32 lo = 2 + decode(smc + 1);
		lo += lo + decode(smc + lo);
		lo += lo + decode(smc + lo);
		lo += lo + decode(smc + lo);
		return (hi << 4) + lo - 0x110;
	}

	/* do decompress */
	__inline__ void decompress() {
		x1 = 0; x2 = 0xFFFFFFFF;
		htbl = mem->htbl;
		ptbl = mem->ptbl;

		fillmem(htbl, HASH_SIZE, (U32) 0);
		fillmem(ptbl, SM_SIZE, (U32) 1 << 31);
		io_init();
		fill_x();

		U32 hash = 0, smc = 0, cxt = 0;
		while (1) {
			U32 chr;
			if (decode(smc) == 0) { // match first?
				chr = cxt & 0xFF;
				cxt += (cxt < MATCH_MAX) ? MATCH_INC : 0; // increment count
			} else if (decode(smc + 1) == 0) { // literal?
				chr = decode_lit(smc + 2);
				if (chr != (cxt & 0xFF)) {
					cxt = ((cxt << 8) & 0xFFFF00) | chr;
				} else {
					break;
				}
			} else if (decode(smc + 2) == 0) { // match second?
				chr = (cxt >> 8) & 0xFF;
				cxt = (cxt & 0xFF0000) | ((cxt << 8) & 0x00FF00) | chr | MATCH_INC;
			} else { // match third?
				chr = (cxt >> 16) & 0xFF;
				cxt = ((cxt << 8) & 0xFFFF00) | chr | MATCH_INC;
			}
			htbl[hash] = cxt;
			hash = (((hash * 5) << HASH_SHIFT) + chr) & HASH_MASK;
			cxt = htbl[hash];
			smc = ((chr << 4) | (cxt >> 24)) * SM_PART;
			_putchr(chr);
		}
		// output remain buffer
		io_final();
	}

public:
	// Compress from in to out. out should be positioned past the header.
	__inline__ box_unbox(
		BOX_CALLBACK_STRUCT *_input, // input
		BOX_CALLBACK_STRUCT *_output, // output
		BOX_WORKMEM *_mem // workmem
	) : box_io(_input, _output), mem(_mem) {
		decompress();
	}
};


// ----------------------------------------------------------------------------
// Implement
__noinline__ unsigned long __cdecl box_workmem_size() {
	return sizeof(BOX_WORKMEM);
}

__noinline__ void __cdecl box_compress(
	BOX_CALLBACK_STRUCT *_input, // input
	BOX_CALLBACK_STRUCT *_output, // output
	void *_mem // // workmem
) {
	box_mkbox mkbox(_input, _output, (BOX_WORKMEM*) _mem);
}

__noinline__ void __cdecl box_decompress(
	BOX_CALLBACK_STRUCT *_input, // input
	BOX_CALLBACK_STRUCT *_output, // output
	void *_mem // // workmem
) {
	box_unbox unbox(_input, _output, (BOX_WORKMEM*) _mem);
}

#endif // _BOX_