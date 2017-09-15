#ifndef _BOX_
#define _BOX_

#include <assert.h>

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
#define __inline__      __inline
#define __forceinline__ __forceinline
#define __noinline__    __declspec(noinline)
#elif defined(__GNUC__)
#define __inline__      __inline
#define __forceinline__ __attribute__((always_inline))
#define __noinline__    __attribute__((noinline))
#endif

#else // NDEBUG undefined

#if defined(_MSC_VER)
#define __inline__      __declspec(noinline)
#define __forceinline__ __declspec(noinline)
#define __noinline__    __declspec(noinline)
#elif defined(__GNUC__)
#define __inline__      __attribute__((noinline))
#define __forceinline__ __attribute__((noinline))
#define __noinline__    __attribute__((noinline))
#endif

#endif // NDEBUG

// ----------------------------------------------------------------------------
// Constant & configure

#define BUFF_BITS 24
#define BUFF_SIZE (1 << BUFF_BITS)
#define BUFF_MASK (BUFF_SIZE - 1)

#define HASH_SHIFT 5
#define HASH_BITS (4 * HASH_SHIFT)
#define HASH_SIZE (1 << HASH_BITS)
#define HASH_MASK (HASH_SIZE - 1)

#define MATCH_BITS 8
#define MATCH_MAXI (1 << MATCH_BITS)
#define MATCH_SIZE (1 << (MATCH_BITS + 1))
#define MATCH_MASK (MATCH_SIZE - 1)
#define MATCH_LOCK (1 << (MATCH_BITS + 2))

#define LIT_BITS 8 // can't be changed
#define LIT_SIZE (1 << LIT_BITS)
#define LIT_MASK (LIT_SIZE - 1)

#define SM_PART (LIT_SIZE + MATCH_MAXI)
#define SM_SIZE (SM_PART * LIT_SIZE)

#define LAZY_BITS (MATCH_BITS + 1)
#define LAZY_SIZE (1 << LAZY_BITS)
#define LAZY_MASK (LAZY_SIZE - 1)

#define LM_SHIFT (LIT_BITS + LIT_BITS)
#define LM_MAXI (MATCH_MAXI << LM_SHIFT)
#define LM_MASK (MATCH_MASK << LM_SHIFT)
#define LM_LOCK (MATCH_LOCK << LM_SHIFT)
#define LM_INCR (1 << LM_SHIFT)


// ----------------------------------------------------------------------------
// Internal typedef

// 8, 16, 32 bit unsigned types (adjust as appropriate)
typedef unsigned char  U8;
typedef unsigned short U16;
typedef unsigned long  U32;

typedef struct _BOX_LAZYSTRUCT {
	U32 len; // hold smc
	U32 ptr;
} BOX_LAZYSTRUCT;

typedef struct _BOX_WORKMEM {
	U32 htbl[HASH_SIZE];
	U32 ptbl[SM_SIZE];
	U8 buffer[BUFF_SIZE];
	BOX_LAZYSTRUCT ltbl[LAZY_SIZE];
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
	__forceinline__ void _fillbuf() {
		(*input->callback)(input);
	}

	/* flush output buffer */
	__forceinline__ void _flushbuf() {
		(*output->callback)(output);
	}

	/* get chr from input */
	__forceinline__ U32 _getchr() {
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
	__forceinline__ void _putchr(U32 chr) {
		if (output->count < output->size) { // is buffer not empty?
			output->buffer[output->count++] = (U8) chr; // put chr to buffer
		} else {
			_flushbuf(); // flush output buffer
			assert(output->size > 0);
			if (output->size > 0) {
				output->count = 0; // reset count
				output->buffer[output->count++] = (U8) chr; // put chr to buffer
			} else {
				// well, dead end here, we lost a chr
			}
		}
	}

	/* io initialize */
	__forceinline__ void io_init() {
		input->count = 0; // reset count
		output->count = 0; // reset count
		_fillbuf();
	}

	/* io finalize */
	__forceinline__ void io_final() {
		if (output->count != 0) {
			_flushbuf();
		}
	}

	__forceinline__ box_io(
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
	U8 *buffer;
	U32 *htbl;
	U32 *ptbl;
	BOX_LAZYSTRUCT* ltbl;

	/*************************************************************
	* Encoder: Arithmetic Encoding using StateMap prediction
	*/

	/* Range, initially [0, 1), scaled by 2^32 */
	U32 x1, x2;

	/* shift out identical leading bytes */
	__forceinline__ void _shift_out() {
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
	__forceinline__ void flush() {
		_putchr(x1 >> 24);
	}

	/* Compress bit (0..1) in context cxt (0..n-1) */
	void code(U32 cxt, U32 bit) {
		U32 p = ptbl[cxt]; // prediction
		ptbl[cxt] += (bit << 25) - (p >> 7); // update prediction
		U32 xmid = x1 + ((x2 - x1) >> 12) * (p >> 20);
		*(bit ? &x2 : &x1) = xmid + (bit ? 0 : 1);
		//bit ? x2 = xmid : x1 = xmid + 1;
		_shift_out();
	}
	/* Compress bit 0 in context cxt (0..n-1) */
	void code_zero(U32 cxt) {
		U32 p = ptbl[cxt]; // prediction
		ptbl[cxt] += (0 << 25) - (p >> 7); // update prediction
		x1 += ((x2 - x1) >> 12) * (p >> 20) + 1;
		_shift_out();
	}
	/* Compress bit 1 in context cxt (0..n-1) */
	void code_one(U32 cxt) {
		U32 p = ptbl[cxt]; // prediction
		ptbl[cxt] += (1 << 25) - (p >> 7); // update prediction
		x2 = x1 + ((x2 - x1) >> 12) * (p >> 20);
		_shift_out();
	}

	/*************************************************************
	* Main compress routine
	*/

	/* code a literal */
	__forceinline__ void code_lit(U32 smc, U32 chr) {
		code_zero(smc);
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

	/* code a match */
	__forceinline__ void code_match(U32 smc, U32 length) {
		assert(length > 0 && length <= MATCH_MAXI);
		code_one(smc);
		smc += 256;
		/*
		0
		100
		101

		*/

		if (length == 1) {
			code_zero(smc);
		} else {
			U32 code_mask = 1;
			for (U32 mask = 1; mask < length; mask += mask + 1) {
				code_one(smc);
				smc += code_mask;
				code_mask += code_mask;
			}
			code_zero(smc);

			if (code_mask < MATCH_MAXI) {
				U32 i = 1;
				do {
					code_mask >>= 1;
					U32 bit = (length & code_mask) ? 1 : 0;
					code(smc + i, bit);
					i += i + bit;
				} while (i <= code_mask);
			}
		}
	}

	/* code a eof */
	__forceinline__ void code_eof(U32 smc) {
		code_one(smc);
		smc += 256;

		for (U32 mask = 1; mask < MATCH_MAXI; mask += mask) {
			code_one(smc);
			smc += mask;
		}
		code_one(smc);
	}

	/* do compress */
	__forceinline__ void compress() {
		x1 = 0; x2 = 0xFFFFFFFF;
		htbl = mem->htbl;
		ptbl = mem->ptbl;
		ltbl = mem->ltbl;
		buffer = mem->buffer;

		fillmem(buffer, BUFF_SIZE, (U8) 0);
		fillmem(htbl, HASH_SIZE, (U32) 0);
		fillmem(ptbl, SM_SIZE, (U32) 1 << 31);
		io_init();

		U32 bufptr = BUFF_MASK;
		U32 hash = 0;

		U32 lpi = 0, lpo = 0, lpsize = 0;
		// state:
		// empty (lpi == lpo)
		// not empty (lpi != lpo)
		// NEVER full

		// current char
		U32 chr;
		while ((chr = _getchr()) <= 0xFF) {
			// lastchar == buffer[bufptr]
			bool is_equal = (buffer[htbl[hash]] == chr);
			ltbl[lpi].len = ((buffer[bufptr] << LIT_BITS) | chr) 
				+ (is_equal ? LM_INCR : LM_LOCK);
			if (is_equal) {
				ltbl[lpi].ptr = (htbl[hash] + 1) & BUFF_MASK;
				lpsize += 1;
			}

			for (
				U32 lpm = (lpi - 1) & LAZY_MASK, cnt = 0;
				lpm != lpo && cnt < lpsize;
				lpm = (lpm - 1) & LAZY_MASK, cnt += 1
			) {
				if (ltbl[lpm].len < LM_MAXI) {
					ltbl[lpm].len += (buffer[ltbl[lpm].ptr] == chr) ? LM_INCR : LM_LOCK;
					ltbl[lpm].ptr = (ltbl[lpm].ptr + 1) & BUFF_MASK;
					if (ltbl[lpm].len >= LM_MAXI) {
						lpsize -= 1;
						cnt -= 1;
					}
				}
			}

			lpi = (lpi + 1) & LAZY_MASK;
			// state:
			// empty (lpi == lpo)
			// not empty (lpi != lpo)
			// NEVER full
			if (lpi == lpo) {
				// full, flush out
				U32 max_score = ltbl[(lpo + 1) & LAZY_MASK].len & LM_MASK;
				U32 max_len = 0;
				for (U32 curr_len = ltbl[lpo].len & LM_MASK; curr_len > 0; curr_len -= LM_INCR) {
					U32 curr_score = curr_len + ltbl[(lpo + curr_len) & LAZY_MASK].len & LM_MASK;
					if (max_score < curr_score) {
						max_score = curr_score;
						max_len = curr_len;
					}
				}

				U32 smc = ((ltbl[lpo].len >> LIT_BITS) & LIT_MASK) * SM_PART;
				if (max_len == 0) {
					// encode literal
					U32 mchr = ltbl[lpo].len & LIT_MASK;
					code_lit(smc, mchr);

					lpo = (lpo + 1) & LAZY_MASK;
				} else {
					// encode match
					U32 mlen = max_len >> LM_SHIFT;
					code_match(smc, mlen);

					lpo = (lpo + mlen) & LAZY_MASK;
				}
			}

			bufptr = (bufptr + 1) & BUFF_MASK;
			htbl[hash] = bufptr;
			hash = (((hash * 5) << HASH_SHIFT) + chr) & HASH_MASK;
			buffer[bufptr] = (U8) chr;
		}
		// mark EOF
		///////////////
		// finalize
		flush();
		io_final();
	}

public:
	// Compress from in to out. out should be positioned past the header.
	__forceinline__ box_mkbox(
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
		U32 p = mem->ptbl[cxt]; // prediction in high 25 bits
		U32 xmid = x1 + ((x2 - x1) >> 12) * (p >> 20);
		U32 bit = (x <= xmid) ? 1 : 0;
		mem->ptbl[cxt] += (bit << 25) - (p >> 7); // update prediction
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
		fillmem(mem->htbl, HASH_SIZE, (U32) 0);
		fillmem(mem->ptbl, SM_SIZE, (U32) 1 << 31);
		io_init();
		fill_x();

		U32 hash = 0, smc = 0, cxt = 0, chr;
		while (1) {



			mem->htbl[hash] = cxt;
			hash = (((hash * 5) << HASH_SHIFT) + chr) & HASH_MASK;
			cxt = mem->htbl[hash];
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