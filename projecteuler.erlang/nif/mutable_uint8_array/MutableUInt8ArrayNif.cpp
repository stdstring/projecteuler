// MutableArrayUInt8Nif.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "erl_nif.h"

#include <cstdint>
#include <cstring>
#include <stdexcept>

static int Load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

//-spec create(Size :: pos_integer(), DefaultValue :: integer()) -> MutableArrayHolder().
static ERL_NIF_TERM Create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

//-spec get(Index :: non_neg_integer(), Array :: MutableArrayHolder()) -> integer().
static ERL_NIF_TERM GetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

//-spec set(Index :: non_neg_integer(), Value :: integer(), Array :: MutableArrayHolder()) -> MutableArrayHolder().
static ERL_NIF_TERM SetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

//-spec is_loaded() -> bool().
static ERL_NIF_TERM IsLoaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

//-spec size(Array :: MutableArrayHolder()) -> pos_integer().
static ERL_NIF_TERM GetSize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc NifFuncs[] =
{
    {"create", 2, Create},
    {"get", 2, GetCellValue},
    {"set", 3, SetCellValue},
    {"is_loaded", 0, IsLoaded},
    {"size", 1, GetSize}
};

ERL_NIF_INIT(mutable_uint8_array, NifFuncs, Load, nullptr, nullptr, nullptr)

struct MutableArrayHolder
{
public:
    uint8_t* Storage;
    int Size;
    uint8_t DefaultValue;

    /*~MutableArrayHolder()
    {
        delete[] Storage;
    }*/
};

static ErlNifResourceType *MutableArrayHolderType = nullptr;

static void MutableArrayHolderDtor(ErlNifEnv* env, void* obj)
{
    //delete obj;
    delete[] static_cast<MutableArrayHolder*>(obj)->Storage;
}

static int Load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    MutableArrayHolderType = enif_open_resource_type(env, "mutable_uint8_array", "MutableArrayHolder", MutableArrayHolderDtor, ErlNifResourceFlags::ERL_NIF_RT_CREATE, nullptr);
    return 0;
}

//-spec create(Size :: pos_integer(), DefaultValue :: integer()) -> MutableArrayHolder().
static ERL_NIF_TERM Create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int size = 0;
    if (!enif_get_int(env, argv[0], &size))
        return enif_make_badarg(env);
    if (size <= 0)
        return enif_make_badarg(env);
    int defaultValue = 0;
    if (!enif_get_int(env, argv[1], &defaultValue))
        return enif_make_badarg(env);
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(enif_alloc_resource(MutableArrayHolderType, sizeof(MutableArrayHolder)));
    mutableArray->Size = size;
    mutableArray->DefaultValue = static_cast<uint8_t>(defaultValue);
    mutableArray->Storage = new uint8_t[size];
    for (int index = 0; index < size; ++index)
        mutableArray->Storage[index] = mutableArray->DefaultValue;
    ERL_NIF_TERM term = enif_make_resource(env, mutableArray);
    // resource now only owned by "Erlang"
    enif_release_resource(mutableArray);
    return term;
}

//-spec get(Index :: non_neg_integer(), Array :: MutableArrayHolder()) -> integer().
static ERL_NIF_TERM GetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* rawResource = nullptr;
    if (!enif_get_resource(env, argv[1], MutableArrayHolderType, &rawResource))
        return enif_make_badarg(env);
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(rawResource);
    int index = 0;
    if (!enif_get_int(env, argv[0], &index))
        return enif_make_badarg(env);
    if ((index < 0) || (index >= mutableArray->Size))
        return enif_make_badarg(env);
    return enif_make_int(env, mutableArray->Storage[index]);
}

//-spec set(Index :: non_neg_integer(), Value :: integer(), Array :: MutableArrayHolder()) -> MutableArrayHolder().
static ERL_NIF_TERM SetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* rawResource = nullptr;
    if (!enif_get_resource(env, argv[2], MutableArrayHolderType, &rawResource))
        return enif_make_badarg(env);
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(rawResource);
    int index = 0;
    if (!enif_get_int(env, argv[0], &index))
        return enif_make_badarg(env);
    if ((index < 0) || (index >= mutableArray->Size))
        return enif_make_badarg(env);
    int value = 0;
    if (!enif_get_int(env, argv[1], &value))
        return enif_make_badarg(env);
    mutableArray->Storage[index] = static_cast<uint8_t>(value);
    return argv[2];
}

//-spec is_loaded() -> bool().
static ERL_NIF_TERM IsLoaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "true");
}

//-spec size(Array :: MutableArrayHolder()) -> pos_integer().
static ERL_NIF_TERM GetSize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* rawResource = nullptr;
    if (!enif_get_resource(env, argv[0], MutableArrayHolderType, &rawResource))
        return enif_make_badarg(env);
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(rawResource);
    return enif_make_int(env, mutableArray->Size);
}
