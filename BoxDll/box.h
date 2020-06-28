#ifndef _BOX_
#define _BOX_

#ifndef _BOX_DLL_
#define _BOX_API_
#else
#ifdef _BOX_EXPORT_
#define _BOX_API_ __declspec(dllexport)
#else
#define _BOX_API_ __declspec(dllimport)
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

// ----------------------------------------------------------------------------
// Type define

typedef struct _BOX_CALLBACK_STRUCT {
	void(__cdecl * callback)(struct _BOX_CALLBACK_STRUCT *);
	void * handle;
	unsigned char * buffer;
	unsigned long size;
	unsigned long count;
} BOX_CALLBACK_STRUCT;

typedef void(__cdecl * BOX_CALLBACK)(struct _BOX_CALLBACK_STRUCT *);

// ----------------------------------------------------------------------------
// Proto define

_BOX_API_ unsigned long __cdecl box_workmem_size();

_BOX_API_ void __cdecl box_compress(
	BOX_CALLBACK_STRUCT * _input, // input
	BOX_CALLBACK_STRUCT * _output, // output
	void * _mem // // workmem
);

_BOX_API_ void __cdecl box_decompress(
	BOX_CALLBACK_STRUCT * _input, // input
	BOX_CALLBACK_STRUCT * _output, // output
	void * _mem // // workmem
);

#ifdef __cplusplus
}
#endif

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// Internal things below, modify at your own risks

#ifdef _BOX_DLL_

#ifdef NDEBUG

#if defined(_MSC_VER)
#define __inline__   __forceinline
#define __noinline__ __declspec(noinline)
#elif defined(__GNUC__)
#define __inline__   __attribute__((always_inline))
#define __noinline__ __attribute__((noinline))
#endif

#ifndef boxAssert
#define boxAssert(x) ;
#endif // !assert

#else // NDEBUG undefined

#if defined(_MSC_VER)
#define __inline__   __declspec(noinline)
#define __noinline__ __declspec(noinline)
#elif defined(__GNUC__)
#define __inline__   __attribute__((noinline))
#define __noinline__ __attribute__((noinline))
#endif

#ifndef boxAssert
#define boxAssert(x) if (!(x)) __debugbreak();
#endif // !assert

#endif // NDEBUG

// ----------------------------------------------------------------------------
// Constant & configure

namespace {

// 8, 16, 32 bit unsigned types (adjust as appropriate)
typedef unsigned char      U8;
typedef unsigned short     U16;
typedef unsigned long      U32;
typedef unsigned long long U64;

constexpr size_t HASH_SHIFT = 6;
constexpr size_t HASH_MUL = 5 << HASH_SHIFT;
constexpr size_t HASH_BITS = 4 * HASH_SHIFT;
constexpr size_t HASH_SIZE = 1 << HASH_BITS;
constexpr size_t HASH_MASK = HASH_SIZE - 1;

constexpr size_t MATCH_CNT = 33;
constexpr size_t MATCH_MAX = (MATCH_CNT - 1) << 24;
constexpr size_t MATCH_INC = 1 << 24;

constexpr size_t MATCH_CNT_SM = 8;

constexpr size_t SM_PART = 258;
constexpr size_t SM_SIZE = SM_PART * 256 * MATCH_CNT_SM;

/*
0 : 0
1 : 1
2 : 2 3
3 : 4 5 6
4 : 7 8 9 10 11
5 : 12 13 14 15 16 17 18 19
6 : 20 21 22 23 24 25 26 27 28 29 30 31 32
7 : 33
*/

static __inline__ size_t matchCount(size_t m) {
	size_t const a = (m < 2) ? m : 2;
	size_t const b = (m < 7) ? 3 : 4;
	size_t const ab = (m < 4) ? a : b;
	size_t const c = (m < 33) ? 6 : 7;
	size_t const cd = (m < 20) ? 5 : c;
	return (m < 12) ? ab : cd;
}

// ----------------------------------------------------------------------------
// Internal typedef

typedef struct alignas(256) _BOX_WORKMEM {
	alignas(256) U32 htbl[HASH_SIZE];
	alignas(256) U32 ptbl[SM_SIZE];
} BOX_WORKMEM;

// ----------------------------------------------------------------------------
// MISC

/* fill memory with pattern. */
template <class type>
static __inline__ void fillmem(type * pointer, size_t size, type fill) {
	for (size_t i = 0; i < size; i++) {
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
	BOX_CALLBACK_STRUCT * input, * output;

	/* fill input buffer */
	__inline__ void _fillbuf() {
		(*input->callback)(input);
	}

	/* flush output buffer */
	__inline__ void _flushbuf() {
		(*output->callback)(output);
	}

	/* is eof */
	__inline__ bool _eof() {
		return input->size == 0;
	}

	/* get chr from input */
	__inline__ size_t _getchr() {
		if (input->count < input->size) { // is buffer not empty?
			return input->buffer[input->count++]; // get chr from buffer
		} else {
			_fillbuf(); // fill input buffer
			if (input->size > 0) {
				input->count = 1; // reset count
				return input->buffer[0]; // get chr from buffer
			} else {
				// we reach the void, _eof == true
				return 0xFF;
			}
			//return (input->size > 0) ? ((input->count = 1), (input->buffer[0])) : 0xFF;
		}
	}

	/* put chr to output */
	__inline__ void _putchr(size_t chr) {
		if (output->count < output->size) { // is buffer not empty?
			output->buffer[output->count++] = (U8)chr; // put chr to buffer
		} else {
			_flushbuf(); // flush output buffer
			if (output->size > 0) {
				output->count = 1; // reset count
				output->buffer[0] = (U8)chr; // put chr to buffer
			} else {
				// well, dead end here, we lost a chr
				__debugbreak();
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
		BOX_CALLBACK_STRUCT * _input, // input
		BOX_CALLBACK_STRUCT * _output // output
	) : input(_input), output(_output) {}
};

// ----------------------------------------------------------------------------
// pconst class

class box_state final {
private:
	/* get pconst */
	static __inline__ int pconst(size_t n) {
		/* i -> round(512.0 / (i+2)) */
		static const U8 box_pconst[128] = {
			255, 170, 128, 102, 85, 73, 64, 56, 51, 46, 42, 39, 36, 34, 32, 30,
			28, 26, 25, 24, 23, 22, 21, 20, 19, 18, 18, 17, 17, 16, 16, 15, 15,
			14, 14, 13, 13, 13, 12, 12, 12, 11, 11, 11, 11, 10, 10, 10, 10, 10,
			9, 9, 9, 9, 9, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 7, 7,
			6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
			5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
			4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3
		};
		return box_pconst[n];
	}

private:
	U32 state;

public:
	__inline__ box_state(U32 _state) : state(_state) {}

	__inline__ size_t get() const {
		return state >> 20;
	}

	__inline__ void update(size_t bit) {
		boxAssert(bit == 0 || bit == 1);
		const U32 p = state;
		const size_t n = p & 127; // count in low 7 bits
		state += ((((bit << 23) - (p >> 9)) * pconst(n)) & 0xFFFFFF80) // update prediction
			+ (n < 127 ? 1 : 0); // increase count
	}

	__inline__ void updateZero() {
		const U32 p = state;
		const size_t n = p & 127; // count in low 7 bits
		state += ((((0 << 23) - (p >> 9)) * pconst(n)) & 0xFFFFFF80) // update prediction
			+ (n < 127 ? 1 : 0); // increase count
	}

	__inline__ void updateOne() {
		const U32 p = state;
		const size_t n = p & 127; // count in low 7 bits
		state += ((((1 << 23) - (p >> 9)) * pconst(n)) & 0xFFFFFF80) // update prediction
			+ (n < 127 ? 1 : 0); // increase count
	}

	__inline__ size_t getAndUpdate(size_t bit) {
		boxAssert(bit == 0 || bit == 1);
		const U32 p = state;
		const size_t n = p & 127; // count in low 7 bits
		state += ((((bit << 23) - (p >> 9)) * pconst(n)) & 0xFFFFFF80) // update prediction
			+ (n < 127 ? 1 : 0); // increase count
		return p >> 20;
	}

	__inline__ size_t getAndUpdateZero() {
		const U32 p = state;
		const size_t n = p & 127; // count in low 7 bits
		state += ((((0 << 23) - (p >> 9)) * pconst(n)) & 0xFFFFFF80) // update prediction
			+ (n < 127 ? 1 : 0); // increase count
		return p >> 20;
	}

	__inline__ size_t getAndUpdateOne() {
		const U32 p = state;
		const size_t n = p & 127; // count in low 7 bits
		state += ((((1 << 23) - (p >> 9)) * pconst(n)) & 0xFFFFFF80) // update prediction
			+ (n < 127 ? 1 : 0); // increase count
		return p >> 20;
	}
};

class box_sm final {
public:
	/* prediction table - secondary context */
	box_state * const ptbl;
	size_t const size;


	/* initialize state map */
	__inline__ box_sm(U32 * _ptbl, size_t _size) : ptbl((box_state *)_ptbl), size(_size) {
		fillmem(_ptbl, _size, (U32)1 << 31);
	}

	/* get state */
	__inline__ box_state & getState(size_t n) {
		boxAssert(n < size);
		return ptbl[n];
	}

	/* get prob */
	__inline__ size_t getProb(size_t n) const {
		boxAssert(n < size);
		return ptbl[n].get();
	}

	__inline__ size_t getAndUpdate(size_t n, size_t bit) {
		boxAssert(n < size);
		boxAssert(bit == 0 || bit == 1);
		return ptbl[n].getAndUpdate(bit);
	}

	__inline__ size_t getAndUpdateZero(size_t n) {
		boxAssert(n < size);
		return ptbl[n].getAndUpdateZero();
	}

	__inline__ size_t getAndUpdateOne(size_t n) {
		boxAssert(n < size);
		return ptbl[n].getAndUpdateOne();
	}
};

// ----------------------------------------------------------------------------
// MKBOX: Compress input to output.

class box_mkbox final : public box_io {
private:
	/* match table & prediction table */
	U32 * const htbl;
	box_sm sm;

	/*************************************************************
	* Encoder: Arithmetic Encoding using StateMap prediction
	*/

	/* Range, initially [0, 1), scaled by 2^32 */
	U32 x1, x2;

	/* shift out identical leading bytes */
	__inline__ void _shift_out() {
		while ((x1 ^ x2) <= 0x00FFFFFF) {  // pass equal leading bytes of range
			_putchr(x1 >> 24);
			x1 <<= 8;
			x2 = (x2 << 8) | 0xFF;
		}
		boxAssert(x1 < x2);
	}

	/* Flush first unequal byte of range */
	__inline__ void flush() {
		_putchr(x1 >> 24);
	}

	/* Compress bit (0..1) in context smc (0..n-1) */
	__inline__ void code(size_t smc, size_t bit) {
		const U32 p = (U32)sm.getAndUpdate(smc, bit);
		const U32 xmid = x1 + ((x2 - x1) >> 12) * p;
		*(bit ? &x2 : &x1) = xmid - (U32)bit + 1;
		boxAssert(x1 < x2);
		_shift_out();
	}

	/* Compress bit 0 in context smc (0..n-1) */
	__inline__ void code_zero(size_t smc) {
		const U32 p = (U32)sm.getAndUpdateZero(smc);
		x1 += ((x2 - x1) >> 12) * p + 1;
		boxAssert(x1 < x2);
		_shift_out();
	}

	/* Compress bit 1 in context smc (0..n-1) */
	__inline__ void code_one(size_t smc) {
		const U32 p = (U32)sm.getAndUpdateOne(smc);
		x2 = x1 + ((x2 - x1) >> 12) * p;
		boxAssert(x1 < x2);
		_shift_out();
	}

	/*************************************************************
	* Main compress routine
	*/

	/* code a literal */
	__noinline__ void code_lit(size_t smc, size_t chr) {
		// code high 4 bits in contexts smc+1..15
		size_t block = ((chr & 0xFF) >> 4) | 16;
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
		io_init();
		size_t hash = 0, smc = 0;
		U32 cxt = 0, * pcxt = &htbl[0];
		while (true) {
			size_t chr = _getchr();
			if (_eof()) break;
			if (chr == (cxt & 0x0000FF)) {  // match first?
				*pcxt += (cxt < MATCH_MAX) ? MATCH_INC : 0;  // increment count
				code_zero(smc);
			} else {
				code_one(smc);
				size_t cmp = (chr * 0x010100) ^ cxt;
				if ((cmp & 0x00FF00) != 0 && (cmp & 0xFF0000) != 0) {  // literal?
					*pcxt = ((cxt << 8) & 0xFFFF00) | (U32)chr;
					code_zero(smc + 1);
					code_lit(smc + 2, chr);
				} else {
					code_one(smc + 1);
					if ((cmp & 0x00FF00) == 0) {  // match second?
						*pcxt = (cxt & 0xFF0000) | ((cxt << 8) & 0x00FF00) | (U32)chr | MATCH_INC;
						code_zero(smc + 2);
					} else {  // match third?
						*pcxt = ((cxt << 8) & 0xFFFF00) | (U32)chr | MATCH_INC;
						code_one(smc + 2);
					}
				}
			}
			hash = (hash * HASH_MUL + chr) & HASH_MASK;
			pcxt = &htbl[hash];
			cxt = *pcxt;
			smc = ((chr << 3) | matchCount(cxt >> 24)) * SM_PART;
		}
		// mark EOF by code first match as literal
		code_one(smc);
		code_zero(smc + 1);
		code_lit(smc + 2, cxt & 0xFF);
		flush();
		io_final();
	}

public:
	// Compress from in to out. out should be positioned past the header.
	__inline__ box_mkbox(
		BOX_CALLBACK_STRUCT * _input, // input
		BOX_CALLBACK_STRUCT * _output, // output
		BOX_WORKMEM * _mem // workmem
	) : box_io(_input, _output),
		x1(0), x2(-1), htbl(_mem->htbl), sm(_mem->ptbl, SM_SIZE) {
		fillmem(htbl, HASH_SIZE, (U32)0x20FF00);
		compress();
	}
};

// ----------------------------------------------------------------------------
// UNBOX: Decompress input to output.

class box_unbox final : public box_io {
private:
	/* match table & prediction table */
	U32 * htbl;
	box_sm sm;

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
			x = (x << 8) | (U32)_getchr();
		}
		boxAssert(x1 <= x && x <= x2 && x1 < x2);
	}

	/* init x for decoding */
	__inline__ void fill_x() {
		for (size_t i = 0; i < sizeof(x); ++i) {
			x = (x << 8) | (U32)_getchr();
		}
	}

	/* Compress bit (0..1) in context smc (0..n-1) */
	__inline__ size_t decode(size_t cxt) {
		box_state & p = sm.getState(cxt);
		const U32 xmid = x1 + ((x2 - x1) >> 12) * (U32)p.get();
		const size_t bit = (x <= xmid) ? 1 : 0;
		p.update(bit);
		*(bit ? &x2 : &x1) = xmid - (U32)bit + 1;
		boxAssert(x1 <= x && x <= x2 && x1 < x2);
		_shift_in();
		return bit;
	}

	/*************************************************************
	* Main decompress routine
	*/

	/* decode a literal */
	__noinline__ size_t decode_lit(size_t smc) {
		// decode high 4 bits in contexts smc+1..15
		size_t hi = 2 + decode(smc + 1);
		hi += hi + decode(smc + hi);
		hi += hi + decode(smc + hi);
		hi += hi + decode(smc + hi);
		// decode low 4 bits in one of 16 blocks of 15 cxts
		smc += 15 * (hi - 15);
		size_t lo = 2 + decode(smc + 1);
		lo += lo + decode(smc + lo);
		lo += lo + decode(smc + lo);
		lo += lo + decode(smc + lo);
		return (hi << 4) + lo - 0x110;
	}

	/* do decompress */
	__inline__ void decompress() {
		io_init();
		fill_x();
		size_t hash = 0, smc = 0;
		U32 cxt = 0, * pcxt = &htbl[0];
		while (true) {
			size_t chr;
			if (decode(smc) == 0) { // match first?
				chr = cxt & 0xFF;
				*pcxt += (cxt < MATCH_MAX) ? MATCH_INC : 0; // increment count
			} else if (decode(smc + 1) == 0) {
				chr = decode_lit(smc + 2);
				if (chr != (cxt & 0xFF)) { // literal?
					*pcxt = ((cxt << 8) & 0xFFFF00) | (U32)chr;
				} else {  // eof
					break;
				}
			} else if (decode(smc + 2) == 0) { // match second?
				chr = (cxt >> 8) & 0xFF;
				*pcxt = (cxt & 0xFF0000) | ((cxt << 8) & 0x00FF00) | (U32)chr | MATCH_INC;
			} else { // match third?
				chr = (cxt >> 16) & 0xFF;
				*pcxt = ((cxt << 8) & 0xFFFF00) | (U32)chr | MATCH_INC;
			}

			hash = (hash * HASH_MUL + chr) & HASH_MASK;
			pcxt = &htbl[hash];
			cxt = *pcxt;
			smc = ((chr << 3) | matchCount(cxt >> 24)) * SM_PART;
			_putchr(chr);
		}
		// output remain buffer
		io_final();
	}

public:
	// Compress from in to out. out should be positioned past the header.
	__inline__ box_unbox(
		BOX_CALLBACK_STRUCT * _input, // input
		BOX_CALLBACK_STRUCT * _output, // output
		BOX_WORKMEM * _mem // workmem
	) : box_io(_input, _output),
		x1(0), x2(-1), htbl(_mem->htbl), sm(_mem->ptbl, SM_SIZE) {
		fillmem(htbl, HASH_SIZE, (U32)0x20FF00);
		decompress();
	}
};

}

// ----------------------------------------------------------------------------
// Implement

#ifdef __cplusplus
extern "C" {
#endif

__noinline__ unsigned long __cdecl box_workmem_size() {
	return sizeof(BOX_WORKMEM);
}

__noinline__ void __cdecl box_compress(
	BOX_CALLBACK_STRUCT * _input, // input
	BOX_CALLBACK_STRUCT * _output, // output
	void * _mem // // workmem
) {
	box_mkbox mkbox(_input, _output, (BOX_WORKMEM *)_mem);
}

__noinline__ void __cdecl box_decompress(
	BOX_CALLBACK_STRUCT * _input, // input
	BOX_CALLBACK_STRUCT * _output, // output
	void * _mem // // workmem
) {
	box_unbox unbox(_input, _output, (BOX_WORKMEM *)_mem);
}

#ifdef __cplusplus
}
#endif

#endif

#endif // _BOX_