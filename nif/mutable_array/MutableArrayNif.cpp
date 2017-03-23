// MutableArrayNif.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "erl_nif.h"

#include <cstring>
#include <stdexcept>

static int Load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

//-spec create(Size :: integer(), DefaultValue :: binary()) -> MutableArrayHolder().
static ERL_NIF_TERM Create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

//-spec get(Array :: MutableArrayHolder(), Index :: non_neg_integer()) -> binary().
static ERL_NIF_TERM GetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

//-spec set(Array :: MutableArrayHolder(), Index :: non_neg_integer(), Value :: binary()) -> MutableArrayHolder().
static ERL_NIF_TERM SetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

//-spec is_loaded() -> bool().
static ERL_NIF_TERM IsLoaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc NifFuncs[] =
{
    {"create", 2, Create},
    {"get", 2, GetCellValue},
    {"set", 3, SetCellValue},
    {"is_loaded", 0, IsLoaded},
};

ERL_NIF_INIT(mutable_array, NifFuncs, Load, nullptr, nullptr, nullptr)

struct MutableArrayItem
{
public:
    ErlNifBinary Data;

    MutableArrayItem()
    {
        Data.data = nullptr;
        Data.size = 0;
    }

    ~MutableArrayItem()
    {
        if (Data.size > 0)
            enif_release_binary(&Data);
    }
};

struct MutableArrayHolder
{
public:
    MutableArrayItem *Storage;
    int Size;
    MutableArrayItem DefaultValue;

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
    MutableArrayHolderType = enif_open_resource_type(env, "mutable_array", "MutableArrayHolder", MutableArrayHolderDtor, ErlNifResourceFlags::ERL_NIF_RT_CREATE, nullptr);
    return 0;
}

//-spec create(Size :: integer(), DefaultValue :: binary()) -> MutableArrayHolder().
static ERL_NIF_TERM Create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int size = 0;
    if (!enif_get_int(env, argv[0], &size))
        return enif_make_badarg(env);
    if (size <= 0)
        return enif_make_badarg(env);
    ErlNifBinary defaultValue;
    if (!enif_inspect_binary(env, argv[1], &defaultValue))
        return enif_make_badarg(env);
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(enif_alloc_resource(MutableArrayHolderType, sizeof(MutableArrayHolder)));
    mutableArray->Size = size;
    if (!enif_alloc_binary(defaultValue.size, &mutableArray->DefaultValue.Data))
        return enif_raise_exception(env, enif_make_atom(env, "internal_error"));
    std::memcpy(mutableArray->DefaultValue.Data.data, defaultValue.data, defaultValue.size);
    mutableArray->Storage = new MutableArrayItem[size];
    ERL_NIF_TERM term = enif_make_resource(env, mutableArray);
    // resource now only owned by "Erlang"
    enif_release_resource(mutableArray);
    return term;
}

//-spec get(Array :: MutableArrayHolder(), Index :: non_neg_integer()) -> binary().
static ERL_NIF_TERM GetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* rawResource = nullptr;
    if (!enif_get_resource(env, argv[0], MutableArrayHolderType, &rawResource))
        return enif_make_badarg(env);
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(rawResource);
    int index = 0;
    if (!enif_get_int(env, argv[1], &index))
        return enif_make_badarg(env);
    if ((index < 0) || (index >= mutableArray->Size))
        return enif_make_badarg(env);
    MutableArrayItem &data = mutableArray->Storage[index].Data.data == nullptr ? mutableArray->DefaultValue : mutableArray->Storage[index];
    ErlNifBinary result;
    enif_alloc_binary(data.Data.size, &result);
    std::memcpy(result.data, data.Data.data, data.Data.size);
    ERL_NIF_TERM term = enif_make_binary(env, &result);
    return term;
}

//-spec set(Array :: MutableArrayHolder(), Index :: non_neg_integer(), Value :: binary()) -> MutableArrayHolder().
static ERL_NIF_TERM SetCellValue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* rawResource = nullptr;
    if (!enif_get_resource(env, argv[0], MutableArrayHolderType, &rawResource))
        return enif_make_badarg(env);
    MutableArrayHolder *mutableArray = static_cast<MutableArrayHolder*>(rawResource);
    int index = 0;
    if (!enif_get_int(env, argv[1], &index))
        return enif_make_badarg(env);
    if ((index < 0) || (index >= mutableArray->Size))
        return enif_make_badarg(env);
    ErlNifBinary value;
    if (!enif_inspect_binary(env, argv[2], &value))
        return enif_make_badarg(env);
    if (mutableArray->Storage[index].Data.size > 0)
        enif_release_binary(&mutableArray->Storage[index].Data);
    if (!enif_alloc_binary(value.size, &mutableArray->Storage[index].Data))
        return enif_raise_exception(env, enif_make_atom(env, "internal_error"));
    std::memcpy(mutableArray->Storage[index].Data.data, value.data, value.size);
    return argv[0];
}

//-spec is_loaded() -> bool().
static ERL_NIF_TERM IsLoaded(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "true");
}
